/// Wrapper over Brotli compression
//
// Brotli is a generic-purpose lossless compression algorithm that compresses
// data using a combination of a modern variant of the LZ77 algorithm,
// Huffman coding and 2nd order context modeling, with a compression ratio
// comparable to the best currently available general-purpose compression
// methods. It is similar in speed with deflate but offers more dense
// compression:
//
// The specification of the Brotli Compressed Data Format is defined in RFC 7932:
// https://tools.ietf.org/html/rfc7932
//
// https://github.com/google/brotli
unit Brotli;

(*
  This file is a path of integration project between HTML5 Boilerplate and
  Synopse mORMot Framework.

    https://synopse.info
    https://html5boilerplate.com

  Boilerplate HTTP Server
  (c) 2016-Present Yevgeny Iliyn

  License: MIT

  https://github.com/eugeneilyin/mORMotBP

  Version 1.9
  - first release, associated with the MormotBP project

  Version 2.3
  - upgrade to Brotli v1.0.9
*)

interface

uses
  SysUtils;

const
  BROTLI_MIN_QUALITY = 0;
  BROTLI_MAX_QUALITY = 11;
  BROTLI_QUALITY_FAST_ONE_PASS_COMPRESSION = 0;
  BROTLI_QUALITY_FAST_TWO_PASS_COMPRESSION = 1;
  BROTLI_QUALITY_ZOPFLIFICATION = 10;
  BROTLI_QUALITY_HQ_ZOPFLIFICATION = 11;
  BROTLI_QUALITY_DEFAULT = BROTLI_QUALITY_HQ_ZOPFLIFICATION;
  BROTLI_QUALITY_COMPRESS_X32 = BROTLI_QUALITY_FAST_ONE_PASS_COMPRESSION;
  BROTLI_QUALITY_COMPRESS_X64 = 3;

  BROTLI_MIN_WINDOW_BITS = 10;
  BROTLI_MAX_WINDOW_BITS = 24;
  BROTLI_WINDOW_DEFAULT = 22;

type
  {$IFDEF FPC}
    {$IFDEF FPC_HAS_CPSTRING}
      // see http://wiki.freepascal.org/FPC_Unicode_support
      {$DEFINE HASCODEPAGE} // UNICODE means {$mode delphiunicode}
    {$ENDIF}
  {$ELSE FPC}
    {$IFDEF UNICODE}
      {$DEFINE HASCODEPAGE}
    {$ENDIF}
  {$ENDIF FPC}
  EncodedString = {$IFDEF HASCODEPAGE}RawByteString{$ELSE}AnsiString{$ENDIF};

  /// Default compression mode
  // - bemGeneric in this mode compressor does not know anything in advance
  // about the properties of the input (default)
  // - bemText compression mode for UTF-8 formatted text input
  // - bemFont compression mode used in WOFF 2.0
  TBrotliEncoderMode = (bemGeneric, bemText, bemFont);

  /// Exception raised internaly in case of Brotli errors
  EBrotliException = class(Exception);

/// Compress data using the Brotli algorithm
// - Data
//     Data to compress
// - Encoded
//     Compressed data
// - Mode
//     Compression mode. See TBrotliEncoderMode.
//     Default: bemGeneric
// - Quality
//     Compression level (0-11); bigger values cause denser,
//     but slower compression.
//       BROTLI_QUALITY_FAST_ONE_PASS_COMPRESSION
//       BROTLI_QUALITY_FAST_TWO_PASS_COMPRESSION
//       BROTLI_QUALITY_ZOPFLIFICATION
//       BROTLI_QUALITY_HQ_ZOPFLIFICATION
//     Range: BROTLI_MIN_QUALITY - BROTLI_MAX_QUALITY (0 - 11)
//     Default: BROTLI_QUALITY_DEFAULT = BROTLI_QUALITY_HQ_ZOPFLIFICATION (11)
// - WindowsBits
//     Set LZ77 window size (0, 10-24); window size is (2**NUM - 16);
//     0 lets compressor decide over the optimal value; bigger windows size
//     improve density; decoder might require up to window size memory
//     to operate.
//     Range: BROTLI_MIN_WINDOW_BITS - BROTLI_MAX_WINDOW_BITS (10 - 24)
//     Default: BROTLI_WINDOW_DEFAULT (22)
// - Returns TRUE on successful compression, or FALSE otherwise
function BrotliCompress(const Data: EncodedString; out Encoded: EncodedString;
  const Mode: TBrotliEncoderMode = bemGeneric;
  const Quality: Integer = BROTLI_QUALITY_DEFAULT;
  const WindowBits: Integer = BROTLI_WINDOW_DEFAULT): Boolean;

/// Performs one-shot memory-to-memory Brotli decompression.
// - Encoded data to decompress
// - Decoded decompressed data
// - Returns TRUE on successful decompression, or FALSE otherwise
function BrotliDecompress(const Encoded: EncodedString;
  out Decoded: EncodedString): Boolean;

type

  // Semantic version, calculated as (MAJOR << 24) | (MINOR << 12) | PATCH
  TBrotliVersion = record
    Major: Cardinal;
    Minor: Cardinal;
    Patch: Cardinal;
  end;

/// Gets an encoder library version.
// Look at TBrotliVersion for more information.
function GetBrotliEncoderVersion: TBrotliVersion;

/// Gets a decoder library version.
// Look at TBrotliVersion for more information.
function GetBrotliDecoderVersion: TBrotliVersion;

/// (Un)Compress a data content using the Brotli algorithm
// - As expected by THttpSocket.RegisterCompress
// - Compared to SynZip.CompressGZip:
//     on x32 platfrom gives the same compression level with 25% more time
//     on x64 platfrom gives ~13.5% better compression for the same time
function CompressBrotli(var DataRawByteString; Compress: Boolean): AnsiString;

implementation

{$IFDEF MSWINDOWS}
  {$IFDEF CPU386}
    {$DEFINE WITH_UNDERSCORE}
    {$L Tools\Brotli\static\bcc32\backward_references.obj}
    {$L Tools\Brotli\static\bcc32\backward_references_hq.obj}
    {$L Tools\Brotli\static\bcc32\bit_cost.obj}
    {$L Tools\Brotli\static\bcc32\bit_reader.obj}
    {$L Tools\Brotli\static\bcc32\block_splitter.obj}
    {$L Tools\Brotli\static\bcc32\brotli_bit_stream.obj}
    {$L Tools\Brotli\static\bcc32\cluster.obj}
    {$L Tools\Brotli\static\bcc32\command.obj}
    {$L Tools\Brotli\static\bcc32\compress_fragment.obj}
    {$L Tools\Brotli\static\bcc32\compress_fragment_two_pass.obj}
    {$L Tools\Brotli\static\bcc32\constants.obj}
    {$L Tools\Brotli\static\bcc32\context.obj}
    {$L Tools\Brotli\static\bcc32\decode.obj}
    {$L Tools\Brotli\static\bcc32\dictionary.obj}
    {$L Tools\Brotli\static\bcc32\dictionary_hash.obj}
    {$L Tools\Brotli\static\bcc32\encode.obj}
    {$L Tools\Brotli\static\bcc32\encoder_dict.obj}
    {$L Tools\Brotli\static\bcc32\entropy_encode.obj}
    {$L Tools\Brotli\static\bcc32\fast_log.obj}
    {$L Tools\Brotli\static\bcc32\histogram.obj}
    {$L Tools\Brotli\static\bcc32\huffman.obj}
    {$L Tools\Brotli\static\bcc32\literal_cost.obj}
    {$L Tools\Brotli\static\bcc32\memory.obj}
    {$L Tools\Brotli\static\bcc32\metablock.obj}
    {$L Tools\Brotli\static\bcc32\platform.obj}
    {$L Tools\Brotli\static\bcc32\state.obj}
    {$L Tools\Brotli\static\bcc32\static_dict.obj}
    {$L Tools\Brotli\static\bcc32\transform.obj}
    {$L Tools\Brotli\static\bcc32\utf8_util.obj}
  {$ELSE CPU386}
    {$L Tools\Brotli\static\bcc64\backward_references.o}
    {$L Tools\Brotli\static\bcc64\backward_references_hq.o}
    {$L Tools\Brotli\static\bcc64\bit_cost.o}
    {$L Tools\Brotli\static\bcc64\bit_reader.o}
    {$L Tools\Brotli\static\bcc64\block_splitter.o}
    {$L Tools\Brotli\static\bcc64\brotli_bit_stream.o}
    {$L Tools\Brotli\static\bcc64\cluster.o}
    {$L Tools\Brotli\static\bcc64\command.o}
    {$L Tools\Brotli\static\bcc64\compress_fragment.o}
    {$L Tools\Brotli\static\bcc64\compress_fragment_two_pass.o}
    {$L Tools\Brotli\static\bcc64\constants.o}
    {$L Tools\Brotli\static\bcc64\context.o}
    {$L Tools\Brotli\static\bcc64\decode.o}
    {$L Tools\Brotli\static\bcc64\dictionary.o}
    {$L Tools\Brotli\static\bcc64\dictionary_hash.o}
    {$L Tools\Brotli\static\bcc64\encode.o}
    {$L Tools\Brotli\static\bcc64\encoder_dict.o}
    {$L Tools\Brotli\static\bcc64\entropy_encode.o}
    {$L Tools\Brotli\static\bcc64\fast_log.o}
    {$L Tools\Brotli\static\bcc64\histogram.o}
    {$L Tools\Brotli\static\bcc64\huffman.o}
    {$L Tools\Brotli\static\bcc64\literal_cost.o}
    {$L Tools\Brotli\static\bcc64\memory.o}
    {$L Tools\Brotli\static\bcc64\metablock.o}
    {$L Tools\Brotli\static\bcc64\platform.o}
    {$L Tools\Brotli\static\bcc64\state.o}
    {$L Tools\Brotli\static\bcc64\static_dict.o}
    {$L Tools\Brotli\static\bcc64\transform.o}
    {$L Tools\Brotli\static\bcc64\utf8_util.o}
  {$ENDIF CPU386}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  {$IFDEF CPU386}
    {$L Tools/Brotli/static/gcc32/backward_references.o}
    {$L Tools/Brotli/static/gcc32/backward_references_hq.o}
    {$L Tools/Brotli/static/gcc32/bit_cost.o}
    {$L Tools/Brotli/static/gcc32/bit_reader.o}
    {$L Tools/Brotli/static/gcc32/block_splitter.o}
    {$L Tools/Brotli/static/gcc32/brotli_bit_stream.o}
    {$L Tools/Brotli/static/gcc32/cluster.o}
    {$L Tools/Brotli/static/gcc32/command.o}
    {$L Tools/Brotli/static/gcc32/compress_fragment.o}
    {$L Tools/Brotli/static/gcc32/compress_fragment_two_pass.o}
    {$L Tools/Brotli/static/gcc32/constants.o}
    {$L Tools/Brotli/static/gcc32/context.o}
    {$L Tools/Brotli/static/gcc32/decode.o}
    {$L Tools/Brotli/static/gcc32/dictionary.o}
    {$L Tools/Brotli/static/gcc32/dictionary_hash.o}
    {$L Tools/Brotli/static/gcc32/encode.o}
    {$L Tools/Brotli/static/gcc32/encoder_dict.o}
    {$L Tools/Brotli/static/gcc32/entropy_encode.o}
    {$L Tools/Brotli/static/gcc32/fast_log.o}
    {$L Tools/Brotli/static/gcc32/histogram.o}
    {$L Tools/Brotli/static/gcc32/huffman.o}
    {$L Tools/Brotli/static/gcc32/literal_cost.o}
    {$L Tools/Brotli/static/gcc32/memory.o}
    {$L Tools/Brotli/static/gcc32/metablock.o}
    {$L Tools/Brotli/static/gcc32/platform.o}
    {$L Tools/Brotli/static/gcc32/state.o}
    {$L Tools/Brotli/static/gcc32/static_dict.o}
    {$L Tools/Brotli/static/gcc32/transform.o}
    {$L Tools/Brotli/static/gcc32/utf8_util.o}
  {$ELSE CPU386}
    {$L Tools/Brotli/static/gcc64/backward_references.o}
    {$L Tools/Brotli/static/gcc64/backward_references_hq.o}
    {$L Tools/Brotli/static/gcc64/bit_cost.o}
    {$L Tools/Brotli/static/gcc64/bit_reader.o}
    {$L Tools/Brotli/static/gcc64/block_splitter.o}
    {$L Tools/Brotli/static/gcc64/brotli_bit_stream.o}
    {$L Tools/Brotli/static/gcc64/cluster.o}
    {$L Tools/Brotli/static/gcc64/command.o}
    {$L Tools/Brotli/static/gcc64/compress_fragment.o}
    {$L Tools/Brotli/static/gcc64/compress_fragment_two_pass.o}
    {$L Tools/Brotli/static/gcc64/constants.o}
    {$L Tools/Brotli/static/gcc64/context.o}
    {$L Tools/Brotli/static/gcc64/decode.o}
    {$L Tools/Brotli/static/gcc64/dictionary.o}
    {$L Tools/Brotli/static/gcc64/dictionary_hash.o}
    {$L Tools/Brotli/static/gcc64/encode.o}
    {$L Tools/Brotli/static/gcc64/encoder_dict.o}
    {$L Tools/Brotli/static/gcc64/entropy_encode.o}
    {$L Tools/Brotli/static/gcc64/fast_log.o}
    {$L Tools/Brotli/static/gcc64/histogram.o}
    {$L Tools/Brotli/static/gcc64/huffman.o}
    {$L Tools/Brotli/static/gcc64/literal_cost.o}
    {$L Tools/Brotli/static/gcc64/memory.o}
    {$L Tools/Brotli/static/gcc64/metablock.o}
    {$L Tools/Brotli/static/gcc64/platform.o}
    {$L Tools/Brotli/static/gcc64/state.o}
    {$L Tools/Brotli/static/gcc64/static_dict.o}
    {$L Tools/Brotli/static/gcc64/transform.o}
    {$L Tools/Brotli/static/gcc64/utf8_util.o}
  {$ENDIF CPU386}
{$ENDIF LINUX}

type
  TBrotliSize = {$IFDEF CPU386} Integer {$ELSE} Int64 {$ENDIF};

  EBrotliExitException = class(EBrotliException)
    Status: Integer;
  end;

{$IFDEF WITH_UNDERSCORE}
function _BrotliEncoderMaxCompressedSize(const InputSize: Integer): Integer;
  cdecl; external;
function _BrotliEncoderCompress(
    const quality: Integer; const lgwin: Integer; const mode: Integer;
    const input_size: TBrotliSize; const input_buffer: Pointer;
    out encoded_size: TBrotliSize; const encoded_buffer: Pointer): Integer;
      cdecl; external;
function _BrotliEncoderVersion: Cardinal; cdecl; external;
function _BrotliDecoderCreateInstance(
  const alloc_func, free_func, opaque: Pointer): Pointer; cdecl; external;
procedure _BrotliDecoderDestroyInstance(const state: Pointer); cdecl; external;
function _BrotliDecoderSetParameter(const state: Pointer;
  const BrotliDecoderParameter: Integer; const Value: Cardinal): Integer;
    cdecl; external;
function _BrotliDecoderDecompressStream(const state: Pointer;
  var available_in: TBrotliSize; var next_in: Pointer;
  var available_out: TBrotliSize; var next_out: Pointer;
  total_out: Pointer): Integer; cdecl; external;
function _BrotliDecoderVersion: Cardinal; cdecl; external;
procedure _BrotliBuildAndStoreHuffmanTreeFast; cdecl; external;
procedure _BrotliBuildHistogramsWithContext; cdecl; external;
procedure _BrotliClusterHistogramsDistance; cdecl; external;
procedure _BrotliClusterHistogramsLiteral; cdecl; external;
procedure _BrotliCompressFragmentFast; cdecl; external;
procedure _BrotliCompressFragmentTwoPass; cdecl; external;
procedure _BrotliCreateBackwardReferences; cdecl; external;
procedure _BrotliCreateHqZopfliBackwardReferences; cdecl; external;
procedure _BrotliCreateZopfliBackwardReferences; cdecl; external;
procedure _BrotliDestroyBlockSplit; cdecl; external;
procedure _BrotliGetDictionary; cdecl; external;
procedure _BrotliInitBitReader; cdecl; external;
procedure _BrotliInitBlockSplit; cdecl; external;
procedure _BrotliInitZopfliNodes; cdecl; external;
procedure _BrotliOptimizeHuffmanCountsForRle; cdecl; external;
procedure _BrotliPopulationCostCommand; cdecl; external;
procedure _BrotliPopulationCostDistance; cdecl; external;
procedure _BrotliPopulationCostLiteral; cdecl; external;
procedure _BrotliSafeReadBits32Slow; cdecl; external;
procedure _BrotliSplitBlock; cdecl; external;
procedure _BrotliStoreHuffmanTree; cdecl; external;
procedure _BrotliStoreMetaBlock; cdecl; external;
procedure _BrotliStoreMetaBlockFast; cdecl; external;
procedure _BrotliStoreMetaBlockTrivial; cdecl; external;
procedure _BrotliStoreUncompressedMetaBlock; cdecl; external;
procedure _BrotliWarmupBitReader; cdecl; external;
procedure _BrotliZopfliComputeShortestPath; cdecl; external;
procedure _BrotliZopfliCreateCommands; cdecl; external;
procedure _kBrotliBitMask; cdecl; external;
procedure _kStaticDictionaryHashWords; cdecl; external;
procedure _kStaticDictionaryHashLengths; cdecl; external;
{$ELSE}
function BrotliEncoderMaxCompressedSize(const InputSize: Integer): Integer;
  cdecl; external;
function BrotliEncoderCompress(
    const quality: Integer; const lgwin: Integer; const mode: Integer;
    const input_size: TBrotliSize; const input_buffer: Pointer;
    out encoded_size: TBrotliSize; const encoded_buffer: Pointer): Integer;
      cdecl; external;
function BrotliEncoderVersion: Cardinal; cdecl; external;
function BrotliDecoderCreateInstance(
  const alloc_func, free_func, opaque: Pointer): Pointer; cdecl; external;
procedure BrotliDecoderDestroyInstance(const state: Pointer); cdecl; external;
function BrotliDecoderSetParameter(const state: Pointer;
  const BrotliDecoderParameter: Integer; const Value: Cardinal): Integer;
    cdecl; external;
function BrotliDecoderDecompressStream(const state: Pointer;
  var available_in: TBrotliSize; var next_in: Pointer;
  var available_out: TBrotliSize; var next_out: Pointer;
  total_out: Pointer): Integer; cdecl; external;
function BrotliDecoderVersion: Cardinal; cdecl; external;
procedure BrotliBuildAndStoreHuffmanTreeFast; cdecl; external;
procedure BrotliBuildHistogramsWithContext; cdecl; external;
procedure BrotliClusterHistogramsDistance; cdecl; external;
procedure BrotliClusterHistogramsLiteral; cdecl; external;
procedure BrotliCompressFragmentFast; cdecl; external;
procedure BrotliCompressFragmentTwoPass; cdecl; external;
procedure BrotliCreateBackwardReferences; cdecl; external;
procedure BrotliCreateHqZopfliBackwardReferences; cdecl; external;
procedure BrotliCreateZopfliBackwardReferences; cdecl; external;
procedure BrotliDestroyBlockSplit; cdecl; external;
procedure BrotliGetDictionary; cdecl; external;
procedure BrotliInitBitReader; cdecl; external;
procedure BrotliInitBlockSplit; cdecl; external;
procedure BrotliInitZopfliNodes; cdecl; external;
procedure BrotliOptimizeHuffmanCountsForRle; cdecl; external;
procedure BrotliPopulationCostCommand; cdecl; external;
procedure BrotliPopulationCostDistance; cdecl; external;
procedure BrotliPopulationCostLiteral; cdecl; external;
procedure BrotliSafeReadBits32Slow; cdecl; external;
procedure BrotliSplitBlock; cdecl; external;
procedure BrotliStoreHuffmanTree; cdecl; external;
procedure BrotliStoreMetaBlock; cdecl; external;
procedure BrotliStoreMetaBlockFast; cdecl; external;
procedure BrotliStoreMetaBlockTrivial; cdecl; external;
procedure BrotliStoreUncompressedMetaBlock; cdecl; external;
procedure BrotliWarmupBitReader; cdecl; external;
procedure BrotliZopfliComputeShortestPath; cdecl; external;
procedure BrotliZopfliCreateCommands; cdecl; external;
procedure kBrotliBitMask; cdecl; external;
procedure kStaticDictionaryHashWords; cdecl; external;
procedure kStaticDictionaryHashLengths; cdecl; external;
{$ENDIF}

const
  EncoderMaxCompressedSize: function(
    const InputSize: Integer): Integer cdecl =
      {$IFDEF WITH_UNDERSCORE}
        _BrotliEncoderMaxCompressedSize
      {$ELSE}
        BrotliEncoderMaxCompressedSize
      {$ENDIF};

  EncoderCompress: function(const quality: Integer; const lgwin: Integer;
    const mode: Integer; const input_size: TBrotliSize;
    const input_buffer: Pointer; out encoded_size: TBrotliSize;
    const encoded_buffer: Pointer): Integer cdecl =
      {$IFDEF WITH_UNDERSCORE}
        _BrotliEncoderCompress
      {$ELSE}
        BrotliEncoderCompress
      {$ENDIF};

  EncoderVersion: function: Cardinal cdecl  =
    {$IFDEF WITH_UNDERSCORE}
      _BrotliEncoderVersion
    {$ELSE}
      BrotliEncoderVersion
    {$ENDIF};

  DecoderCreateInstance: function(const alloc_func, free_func,
    opaque: Pointer): Pointer cdecl =
      {$IFDEF WITH_UNDERSCORE}
        _BrotliDecoderCreateInstance
      {$ELSE}
        BrotliDecoderCreateInstance
      {$ENDIF};

  DecoderDestroyInstance: procedure(const state: Pointer) cdecl =
    {$IFDEF WITH_UNDERSCORE}
      _BrotliDecoderDestroyInstance
    {$ELSE}
      BrotliDecoderDestroyInstance
    {$ENDIF};

  DecoderSetParameter: function (const state: Pointer;
    const BrotliDecoderParameter: Integer;
    const Value: Cardinal): Integer cdecl =
      {$IFDEF WITH_UNDERSCORE}
        _BrotliDecoderSetParameter
      {$ELSE}
        BrotliDecoderSetParameter
      {$ENDIF};

  DecoderDecompressStream: function(const state: Pointer;
    var available_in: TBrotliSize; var next_in: Pointer;
    var available_out: TBrotliSize; var next_out: Pointer;
    total_out: Pointer = nil): Integer cdecl =
      {$IFDEF WITH_UNDERSCORE}
        _BrotliDecoderDecompressStream
      {$ELSE}
        BrotliDecoderDecompressStream
      {$ENDIF};

  DecoderVersion: function: Cardinal cdecl  =
    {$IFDEF WITH_UNDERSCORE}
      _BrotliDecoderVersion
    {$ELSE}
      BrotliDecoderVersion
    {$ENDIF};

function BrotliCompress(const Data: EncodedString; out Encoded: EncodedString;
  const Mode: TBrotliEncoderMode; const Quality, WindowBits: Integer): Boolean;
const
  BROTLI_MODE_GENERIC = 0;
  BROTLI_MODE_TEXT = 1;
  BROTLI_MODE_FONT = 2;
  BROTLI_TRUE = 1;
  MODE_VALUES: array[TBrotliEncoderMode] of Integer =
    (BROTLI_MODE_GENERIC, BROTLI_MODE_TEXT, BROTLI_MODE_FONT);
var
  EncodedSize: TBrotliSize;
begin
  try
    EncodedSize := EncoderMaxCompressedSize(Length(Data));
    Result := EncodedSize > 0;
    if not Result then Exit;
    SetLength(Encoded, EncodedSize);
    Result := EncoderCompress(Quality, WindowBits, MODE_VALUES[Mode],
      Length(Data), Pointer(Data), EncodedSize, Pointer(Encoded)) = BROTLI_TRUE;
    if Result then
      SetLength(Encoded, EncodedSize);
  except
    on EBrotliException do
      Result := False;
  end;
end;

function BrotliDecompress(const Encoded: EncodedString;
  out Decoded: EncodedString): Boolean;
const
  BUFFER_SIZE = 1 shl 19;
  BROTLI_DECODER_PARAM_LARGE_WINDOW = 1;
  BROTLI_DECODER_RESULT_ERROR = 0;
  BROTLI_DECODER_RESULT_SUCCESS = 1;
  BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT = 2;
  BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = 3;
var
  State: Pointer;
  DecoderResult: Integer;
  AvailableIn: TBrotliSize;
  NextIn: Pointer;
  AvailableOut: TBrotliSize;
  NextOut: Pointer;
  BufferOut: array of Byte;

  procedure WriteOutput;
  var
    OutSize: TBrotliSize;
    PrevLen: Integer;
  begin
    OutSize := PAnsiChar(NextOut) - PAnsiChar(@BufferOut[0]);
    if OutSize = 0 then Exit;

    PrevLen := Length(Decoded);
    SetLength(Decoded, PrevLen + OutSize);
    Move(BufferOut[0], Decoded[PrevLen + 1], OutSize);
  end;

begin
  State := DecoderCreateInstance(nil, nil, nil);
  if State = nil then
  begin
    Result := False;
    Exit;
  end;

  try
    if DecoderSetParameter(State, BROTLI_DECODER_PARAM_LARGE_WINDOW, 1) = 0 then
    begin
      Result := False;
      Exit;
    end;

    AvailableIn := Length(Encoded);
    NextIn := Pointer(Encoded);
    AvailableOut := BUFFER_SIZE;
    SetLength(BufferOut, BUFFER_SIZE);
    NextOut := @BufferOut[0];
    SetLength(Decoded, 0);

    while True do
    begin
      DecoderResult := DecoderDecompressStream(
        State, AvailableIn, NextIn, AvailableOut, NextOut);
      case DecoderResult of
        BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT:
          begin
            WriteOutput;
            AvailableOut := BUFFER_SIZE;
            NextOut := @BufferOut[0];
          end;

        BROTLI_DECODER_RESULT_SUCCESS:
          begin
            WriteOutput;
            Result := True;
            Exit;
          end;

        else begin
          Result := False;
          Exit;
        end;
      end;
    end;
  finally
    DecoderDestroyInstance(State);
  end;
end;

function ParseVersion(const Version: Cardinal): TBrotliVersion;
begin
  Result.Major := Version shr 24;
  Result.Minor := Version and $FFF000 shr 12;
  Result.Patch := Version and $FFF;
end;

function GetBrotliEncoderVersion: TBrotliVersion;
begin
  Result := ParseVersion(EncoderVersion);
end;

function GetBrotliDecoderVersion: TBrotliVersion;
begin
  Result := ParseVersion(DecoderVersion);
end;

function CompressBrotli(var DataRawByteString; Compress: Boolean): AnsiString;
var
  Data: EncodedString absolute DataRawByteString;
  Temp: EncodedString;
begin
  if Compress then
  begin
    if not BrotliCompress(Data, Temp, bemText,
      {$IFDEF CPU386}
        BROTLI_QUALITY_COMPRESS_X32
      {$ELSE}
        BROTLI_QUALITY_COMPRESS_X64
      {$ENDIF}) then
        raise EBrotliException.Create('Error during Brotli compression');
  end else
    if not BrotliDecompress(Data, Temp) then
      raise EBrotliException.Create('Error during Brotli decompression');
  Data := Temp;
  Result := 'br';
end;

{$IFNDEF FPC}
{$IFDEF WITH_UNDERSCORE}
procedure __exit(Status: Integer); cdecl; export;
{$ELSE}
procedure _exit(Status: Integer); cdecl; export;
{$ENDIF}
var
  E: EBrotliExitException;
begin
  E := EBrotliExitException.CreateFmt('Brotli exit: Status=%d', [Status]);
  E.Status := Status;
  raise E;
end;
{$ENDIF}

// See https://github.com/caodj/dSqlite3/blob/master/src/sqlite3/MSVCRT.pas

{$IFDEF WITH_UNDERSCORE}
function _malloc(Size: Cardinal): Pointer; cdecl;
{$ELSE}
function malloc(Size: Cardinal): Pointer; cdecl;
{$ENDIF}
begin
  GetMem(Result, Size);
end;

{$IFDEF WITH_UNDERSCORE}
procedure _free(P: Pointer); cdecl;
{$ELSE}
procedure free(P: Pointer); cdecl;
{$ENDIF}
begin
  FreeMem(P);
end;

{$IFDEF WITH_UNDERSCORE}
function _memset(P: Pointer; B: Integer; Count: Integer): Pointer; cdecl;
{$ELSE}
function memset(P: Pointer; B: Integer; Count: Integer): Pointer; cdecl;
{$ENDIF}
begin
  FillChar(P^, Count, B);
  Result := P;
end;

{$IFDEF WITH_UNDERSCORE}
function _memcpy(Dest, Source: Pointer; Count: TBrotliSize): Pointer; cdecl;
{$ELSE}
function memcpy(Dest, Source: Pointer; Count: TBrotliSize): Pointer; cdecl;
{$ENDIF}
begin
  Move(Source^, Dest^, Count);
  Result := Dest;
end;

{$IFDEF WITH_UNDERSCORE}
function _memmove(Dest, Source: Pointer; const Count: Integer): Pointer; cdecl;
{$ELSE}
function memmove(Dest, Source: Pointer; const Count: Integer): Pointer; cdecl;
{$ENDIF}
begin
  Move(source^, dest^, count);
  result := dest;
end;

{$IFDEF WIN32}
function _brotliLog(X: Double): Double; cdecl; export;
{$ENDIF WIN32}
{$IFDEF WIN64}
function _brotliLog(X: Double): Double; cdecl; export;
{$ENDIF WIN64}
{$IFDEF FPC}{$IFDEF LINUX}
function brotliLog(X: Double): Double; cdecl; export;
{$ENDIF}
{$ENDIF FPC}
begin
  Result := Ln(X);
end;
{$IFDEF WIN64}
function brotliLog(X: Double): Double; cdecl; export;
begin
  Result := Ln(X);
end;
{$ENDIF WIN64}

{$IFNDEF FPC}

{$IFDEF WIN32}
procedure __llushr;
asm
  jmp System.@_llushr
end;

procedure __llmul;
asm
  jmp System.@_llmul
end;

// Borland C++ float to integer (Int64) conversion
function __ftol: Int64;
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

procedure __llshl;
asm
  jmp System.@_llshl
end;

const
  __huge_dble: Double = 1e300;
var
  __turboFloat: Word; { not used, but must be present for linking }

{$ENDIF WIN32}

{$IFDEF WIN64}
/// Allocate temporary stack memory
//
// __chkstk is a helper function for the compiler. It is called in the prologue
// to a function that has more than 4K bytes of local variables. It performs a
// stack probe by poking all pages in the new stack area. The number of bytes
// that will be allocated is passed in RAX.
//
// See $(BDS)\source\cpprtl\Source\memory\chkstk.nasm
procedure __chkstk;
asm
        lea     r10, [rsp]
        mov     r11, r10
        sub     r11, rax
        and     r11w, 0f000h
        and     r10w, 0f000h
@loop:  sub     r10, 01000h
        cmp     r10, r11       // more to go?
        jl      @exit
        mov     qword [r10], 0 // probe this page
        jmp     @loop
@exit:
end;

const
  _Inf: Double = 1e300;
{$ENDIF WIN64}

{$ENDIF FPC}

end.

