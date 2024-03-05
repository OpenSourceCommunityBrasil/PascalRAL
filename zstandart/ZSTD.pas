unit ZSTD;

interface

uses
  Windows, Classes, ZSTDLib;

type
  TZSTDCompressOptions = record
    CompressionLevel: Integer; (* Update all compression parameters according to pre-defined cLevel table
                              * Default level is ZSTD_CLEVEL_DEFAULT==3.
                              * Special: value 0 means default, which is controlled by ZSTD_CLEVEL_DEFAULT.
                              * Note 1 : it's possible to pass a negative compression level.
                              * Note 2 : setting a level sets all default values of other compression parameters *)
    WindowLog: Integer;      (* Maximum allowed back-reference distance, expressed as power of 2.
                              * Must be clamped between ZSTD_WINDOWLOG_MIN and ZSTD_WINDOWLOG_MAX.
                              * Special: value 0 means "use default windowLog".
                              * Note: Using a windowLog greater than ZSTD_WINDOWLOG_LIMIT_DEFAULT
                              *       requires explicitly allowing such window size at decompression stage if using streaming. *)
    HashLog: Integer;        (* Size of the initial probe table, as a power of 2.
                              * Resulting memory usage is (1 << (hashLog+2)).
                              * Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX.
                              * Larger tables improve compression ratio of strategies <= dFast,
                              * and improve speed of strategies > dFast.
                              * Special: value 0 means "use default hashLog". *)
    ChainLog: Integer;       (* Size of the multi-probe search table, as a power of 2.
                              * Resulting memory usage is (1 << (chainLog+2)).
                              * Must be clamped between ZSTD_CHAINLOG_MIN and ZSTD_CHAINLOG_MAX.
                              * Larger tables result in better and slower compression.
                              * This parameter is useless when using "fast" strategy.
                              * It's still useful when using "dfast" strategy,
                              * in which case it defines a secondary probe table.
                              * Special: value 0 means "use default chainLog". *)
    SearchLog: Integer;      (* Number of search attempts, as a power of 2.
                              * More attempts result in better and slower compression.
                              * This parameter is useless when using "fast" and "dFast" strategies.
                              * Special: value 0 means "use default searchLog". *)
    MinMatch: Integer;       (* Minimum size of searched matches.
                              * Note that Zstandard can still find matches of smaller size,
                              * it just tweaks its search algorithm to look for this size and larger.
                              * Larger values increase compression and decompression speed, but decrease ratio.
                              * Must be clamped between ZSTD_MINMATCH_MIN and ZSTD_MINMATCH_MAX.
                              * Note that currently, for all strategies < btopt, effective minimum is 4.
                              *                    , for all strategies > fast, effective maximum is 6.
                              * Special: value 0 means "use default minMatchLength". *)
    TargetLength: Integer;   (* Impact of this field depends on strategy.
                              * For strategies btopt, btultra & btultra2:
                              *     Length of Match considered "good enough" to stop search.
                              *     Larger values make compression stronger, and slower.
                              * For strategy fast:
                              *     Distance between match sampling.
                              *     Larger values make compression faster, and weaker.
                              * Special: value 0 means "use default targetLength". *)
    Strategy: Integer;       (* See ZSTD_strategy enum definition.
                              * The higher the value of selected strategy, the more complex it is,
                              * resulting in stronger and slower compression.
                              * Special: value 0 means "use default strategy". *)

    (* LDM mode parameters *)
    EnableLongDistanceMatching: Boolean; (* Enable long distance matching.
                                     * This parameter is designed to improve compression ratio
                                     * for large inputs, by finding large matches at long distance.
                                     * It increases memory usage and window size.
                                     * Note: enabling this parameter increases default ZSTD_c_windowLog to 128 MB
                                     * except when expressly set to a different value. *)
    LdmHashLog: Integer;     (* Size of the table for long distance matching, as a power of 2.
                              * Larger values increase memory usage and compression ratio,
                              * but decrease compression speed.
                              * Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX
                              * default: windowlog - 7.
                              * Special: value 0 means "automatically determine hashlog". *)
    LdmMinMatch: Integer;    (* Minimum match size for long distance matcher.
                              * Larger/too small values usually decrease compression ratio.
                              * Must be clamped between ZSTD_LDM_MINMATCH_MIN and ZSTD_LDM_MINMATCH_MAX.
                              * Special: value 0 means "use default value" (default: 64). *)
    LdmBucketSizeLog: Integer; (* Log size of each bucket in the LDM hash table for collision resolution.
                              * Larger values improve collision resolution but decrease compression speed.
                              * The maximum value is ZSTD_LDM_BUCKETSIZELOG_MAX.
                              * Special: value 0 means "use default value" (default: 3). *)
    LdmHashRateLog: Integer; (* Frequency of inserting/looking up entries into the LDM hash table.
                              * Must be clamped between 0 and (ZSTD_WINDOWLOG_MAX - ZSTD_HASHLOG_MIN).
                              * Default is MAX(0, (windowLog - ldmHashLog)), optimizing hash table usage.
                              * Larger values improve compression speed.
                              * Deviating far from default value will likely result in a compression ratio decrease.
                              * Special: value 0 means "automatically determine hashRateLog". *)

    (* frame parameters *)
    ContentSizeFlag: Boolean; (* Content size will be written into frame header _whenever known_ (default:1)
                              * Content size must be known at the beginning of compression.
                              * This is automatically the case when using ZSTD_compress2(),
                              * For streaming variants, content size must be provided with ZSTD_CCtx_setPledgedSrcSize() *)
    ChecksumFlag: Boolean;   (* A 32-bits checksum of content is written at end of frame (default:0) *)
    DictIDFlag: Boolean;     (* When applicable, dictionary's ID is written into frame header (default:1) *)

    (* multi-threading parameters *)
    (* These parameters are only useful if multi-threading is enabled (compiled with build macro ZSTD_MULTITHREAD).
     * They return an error otherwise. *)
    Workers: Integer;        (* Select how many threads will be spawned to compress in parallel.
                              * When nbWorkers >= 1, triggers asynchronous mode when used with ZSTD_compressStream*() :
                              * ZSTD_compressStream*() consumes input and flush output if possible, but immediately gives back control to caller,
                              * while compression work is performed in parallel, within worker threads.
                              * (note : a strong exception to this rule is when first invocation of ZSTD_compressStream2() sets ZSTD_e_end :
                              *  in which case, ZSTD_compressStream2() delegates to ZSTD_compress2(), which is always a blocking call).
                              * More workers improve speed, but also increase memory usage.
                              * Default value is `0`, aka "single-threaded mode" : no worker is spawned, compression is performed inside Caller's thread, all invocations are blocking *)
    JobSize: Integer;        (* Size of a compression job. This value is enforced only when nbWorkers >= 1.
                              * Each compression job is completed in parallel, so this value can indirectly impact the nb of active threads.
                              * 0 means default, which is dynamically determined based on compression parameters.
                              * Job size must be a minimum of overlap size, or 1 MB, whichever is largest.
                              * The minimum size is automatically and transparently enforced *)
    OverlapLog: Integer;     (* Control the overlap size, as a fraction of window size.
                              * The overlap size is an amount of data reloaded from previous job at the beginning of a new job.
                              * It helps preserve compression ratio, while each job is compressed in parallel.
                              * This value is enforced only when nbWorkers >= 1.
                              * Larger values increase compression ratio, but decrease speed.
                              * Possible values range from 0 to 9 :
                              * - 0 means "default" : value will be determined by the library, depending on strategy
                              * - 1 means "no overlap"
                              * - 9 means "full overlap", using a full window size.
                              * Each intermediate rank increases/decreases load size by a factor 2 :
                              * 9: full window;  8: w/2;  7: w/4;  6: w/8;  5:w/16;  4: w/32;  3:w/64;  2:w/128;  1:no overlap;  0:default
                              * default value varies between 6 and 9, depending on strategy *)
    procedure Init;
    function ParamMin(AParam: ZSTD_cParameter): Integer; inline;
    function ParamMax(AParam: ZSTD_cParameter): Integer; inline;
    function CompressionLevelMin: Integer; inline;
    function CompressionLevelMax: Integer; inline;
    function WindowLogMin: Integer; inline;
    function WindowLogMax: Integer; inline;
    function HashLogMin: Integer; inline;
    function HashLogMax: Integer; inline;
    function ChainLogMin: Integer; inline;
    function ChainLogMax: Integer; inline;
    function SearchLogMin: Integer; inline;
    function SearchLogMax: Integer; inline;
    function MinMatchMin: Integer; inline;
    function MinMatchMax: Integer; inline;
    function TargetLengthMin: Integer; inline;
    function TargetLengthMax: Integer; inline;
    function StrategyMin: Integer; inline;
    function StrategyMax: Integer; inline;
    function LdmHashLogMin: Integer; inline;
    function LdmHashLogMax: Integer; inline;
    function LdmMinMatchMin: Integer; inline;
    function LdmMinMatchMax: Integer; inline;
    function LdmBucketSizeLogMin: Integer; inline;
    function LdmBucketSizeLogMax: Integer; inline;
    function LdmHashRateLogMin: Integer; inline;
    function LdmHashRateLogMax: Integer; inline;
    function WorkersMin: Integer; inline;
    function WorkersMax: Integer; inline;
    function JobSizeMin: Integer; inline;
    function JobSizeMax: Integer; inline;
    function OverlapLogMin: Integer; inline;
    function OverlapLogMax: Integer; inline;
    procedure Check;
  end;

  TZSTDDecompressOptions = record
    WindowLog: Integer;       (* Select a size limit (in power of 2) beyond which
                              * the streaming API will refuse to allocate memory buffer
                              * in order to protect the host from unreasonable memory requirements.
                              * This parameter is only useful in streaming mode, since no internal buffer is allocated in single-pass mode.
                              * By default, a decompression context accepts window sizes <= (1 << ZSTD_WINDOWLOG_LIMIT_DEFAULT).
                              * Special: value 0 means "use default maximum windowLog". *)
    procedure Init;
    function ParamMin(AParam: ZSTD_dParameter): Integer; inline;
    function ParamMax(AParam: ZSTD_dParameter): Integer; inline;
    function WindowLogMin: Integer; inline;
    function WindowLogMax: Integer; inline;
    procedure Check;
  end;

procedure ZSTDCompressStream(ASource, ADest: TStream; const AOption: TZSTDCompressOptions; ACount: Int64 = 0); overload;
procedure ZSTDCompressStream(ASource, ADest: TStream; ALevel: Integer; AThreadCount: Integer = 0; ACount: Int64 = 0); overload;
procedure ZSTDDecompressStream(ASource, ADest: TStream; const AOption: TZSTDDecompressOptions; ACount: Int64 = 0); overload;
procedure ZSTDDecompressStream(ASource, ADest: TStream; ACount: Int64 = 0); overload;

type
  TZSTDCompressStream = class(TStream)
  public
    class function MaxLevel: Integer;
    constructor Create(ADest: TStream; ALevel: Integer; AThreadCount: Integer = 0); overload; // ALevel in range 1-MaxLevel
    constructor Create(ADest: TStream; const AOption: TZSTDCompressOptions); overload;
    destructor Destroy; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
  private
    FDest: TStream;
    FOptions: TZSTDCompressOptions;
    FStreamOutBufferSize: size_t;
    FStreamOutBuffer: Pointer;
    FStream: ZSTD_CStream;
  end;

  TZSTDDecompressStream = class(TStream)
  public
    constructor Create(ASource: TStream); overload;
    constructor Create(ASource: TStream; const AOption: TZSTDDecompressOptions); overload;
    destructor Destroy; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
  private
    FSource: TStream;
    FOptions: TZSTDDecompressOptions;
    FPosition: Int64;
    FStreamInBufferSize: size_t;
    FStreamInBuffer: Pointer;
    FInput: ZSTD_inBuffer;
    FStreamOutBufferSize: size_t;
    FStreamOutBuffer: Pointer;
    FStreamOutBufferSizePos: size_t;
    FOutput: ZSTD_outBuffer;
    FStream: ZSTD_DStream;
    FSeekBuffer: Pointer;
  end;

implementation

uses
  SysUtils;

const
  CopyBufferSize = 65536;

procedure ZSTDCompressStream(ASource, ADest: TStream; const AOption: TZSTDCompressOptions; ACount: Int64 = 0);
var
  Buffer: Pointer;
  CompressStream: TStream;
  FullStream: Boolean;
  Read: Integer;
begin
  GetMem(Buffer, CopyBufferSize);
  try
    CompressStream := TZSTDCompressStream.Create(ADest, AOption);
    try
      FullStream := ACount = 0;
      while True do
        begin
          if FullStream then
            begin
              Read := ASource.Read(Buffer^, CopyBufferSize);
              if Read = 0 then Break;
              CompressStream.WriteBuffer(Buffer^, Read);
            end
          else
            begin
              if ACount > CopyBufferSize then Read := CopyBufferSize
                                         else Read := ACount;
              ASource.ReadBuffer(Buffer^, Read);
              CompressStream.WriteBuffer(Buffer^, Read);
              Dec(ACount, Read);
              if ACount = 0 then Break;
            end;
        end;
    finally
      CompressStream.Free;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure ZSTDCompressStream(ASource, ADest: TStream; ALevel: Integer; AThreadCount: Integer = 0; ACount: Int64 = 0);
var
  Options: TZSTDCompressOptions;
begin
  Options.Init;
  Options.CompressionLevel := ALevel;
  Options.Workers := AThreadCount;
  Options.Check;
  ZSTDCompressStream(ASource, ADest, Options);
end;

procedure ZSTDDecompressStream(ASource, ADest: TStream; const AOption: TZSTDDecompressOptions; ACount: Int64 = 0);
var
  Buffer: Pointer;
  DecompressStream: TStream;
  FullStream: Boolean;
  Read: Integer;
begin
  GetMem(Buffer, CopyBufferSize);
  try
    DecompressStream := TZSTDDecompressStream.Create(ASource, AOption);
    try
      FullStream := ACount = 0;
      while True do
        begin
          if FullStream then
            begin
              Read := DecompressStream.Read(Buffer^, CopyBufferSize);
              if Read = 0 then Break;
              ADest.WriteBuffer(Buffer^, Read);
            end
          else
            begin
              if ACount > CopyBufferSize then Read := CopyBufferSize
                                         else Read := ACount;
              DecompressStream.ReadBuffer(Buffer^, Read);
              ADest.WriteBuffer(Buffer^, Read);
              Dec(ACount, Read);
              if ACount = 0 then Break;
            end;
        end;
    finally
      DecompressStream.Free;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure ZSTDDecompressStream(ASource, ADest: TStream; ACount: Int64 = 0);
var
  Option: TZSTDDecompressOptions;
begin
  Option.Init;
  ZSTDDecompressStream(ASource, ADest, Option, ACount);
end;

//**************************************************************************************************
// TZSTDCompressOptions
//**************************************************************************************************

procedure TZSTDCompressOptions.Init;
begin
  ZeroMemory(@Self, SizeOf(Self));
  CompressionLevel := ZSTD_CLEVEL_DEFAULT;
  DictIDFlag := True;
end;

function TZSTDCompressOptions.ParamMin(AParam: ZSTD_cParameter): Integer;
var
  Bounds: ZSTD_bounds;
begin
  Bounds := ZSTD_cParam_getBounds(AParam);
  ZSTDCheck(sZSTD_cParam_getBounds, Bounds.error);
  Result := Bounds.lowerBound;
end;

function TZSTDCompressOptions.ParamMax(AParam: ZSTD_cParameter): Integer;
var
  Bounds: ZSTD_bounds;
begin
  Bounds := ZSTD_cParam_getBounds(AParam);
  ZSTDCheck(sZSTD_cParam_getBounds, Bounds.error);
  Result := Bounds.upperBound;
end;

function TZSTDCompressOptions.CompressionLevelMin: Integer;
begin
  Result := ParamMin(ZSTD_c_compressionLevel);
end;

function TZSTDCompressOptions.CompressionLevelMax: Integer;
begin
  Result := ParamMax(ZSTD_c_compressionLevel);
end;

function TZSTDCompressOptions.WindowLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_windowLog);
end;

function TZSTDCompressOptions.WindowLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_windowLog);
end;

function TZSTDCompressOptions.HashLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_hashLog);
end;

function TZSTDCompressOptions.HashLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_hashLog);
end;

function TZSTDCompressOptions.ChainLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_chainLog);
end;

function TZSTDCompressOptions.ChainLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_chainLog);
end;

function TZSTDCompressOptions.SearchLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_searchLog);
end;

function TZSTDCompressOptions.SearchLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_searchLog);
end;

function TZSTDCompressOptions.MinMatchMin: Integer;
begin
  Result := ParamMin(ZSTD_c_minMatch);
end;

function TZSTDCompressOptions.MinMatchMax: Integer;
begin
  Result := ParamMax(ZSTD_c_minMatch);
end;

function TZSTDCompressOptions.TargetLengthMin: Integer;
begin
  Result := ParamMin(ZSTD_c_targetLength);
end;

function TZSTDCompressOptions.TargetLengthMax: Integer;
begin
  Result := ParamMax(ZSTD_c_targetLength);
end;

function TZSTDCompressOptions.StrategyMin: Integer;
begin
  Result := ParamMin(ZSTD_c_strategy);
end;

function TZSTDCompressOptions.StrategyMax: Integer;
begin
  Result := ParamMax(ZSTD_c_strategy);
end;

function TZSTDCompressOptions.LdmHashLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_ldmHashLog);
end;

function TZSTDCompressOptions.LdmHashLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_ldmHashLog);
end;

function TZSTDCompressOptions.LdmMinMatchMin: Integer;
begin
  Result := ParamMin(ZSTD_c_ldmMinMatch);
end;

function TZSTDCompressOptions.LdmMinMatchMax: Integer;
begin
  Result := ParamMax(ZSTD_c_ldmMinMatch);
end;

function TZSTDCompressOptions.LdmBucketSizeLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_ldmBucketSizeLog);
end;

function TZSTDCompressOptions.LdmBucketSizeLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_ldmBucketSizeLog);
end;

function TZSTDCompressOptions.LdmHashRateLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_ldmHashRateLog);
end;

function TZSTDCompressOptions.LdmHashRateLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_ldmHashRateLog);
end;

function TZSTDCompressOptions.WorkersMin: Integer;
begin
  Result := ParamMin(ZSTD_c_nbWorkers);
end;

function TZSTDCompressOptions.WorkersMax: Integer;
begin
  Result := ParamMax(ZSTD_c_nbWorkers);
end;

function TZSTDCompressOptions.JobSizeMin: Integer;
begin
  Result := ParamMin(ZSTD_c_jobSize);
end;

function TZSTDCompressOptions.JobSizeMax: Integer;
begin
  Result := ParamMax(ZSTD_c_jobSize);
end;

function TZSTDCompressOptions.OverlapLogMin: Integer;
begin
  Result := ParamMin(ZSTD_c_overlapLog);
end;

function TZSTDCompressOptions.OverlapLogMax: Integer;
begin
  Result := ParamMax(ZSTD_c_overlapLog);
end;

procedure TZSTDCompressOptions.Check;
begin
  if
    ((CompressionLevel <> 0) and ((CompressionLevel < CompressionLevelMin) or (CompressionLevel > CompressionLevelMax))) or
    ((WindowLog        <> 0) and ((WindowLog        < WindowLogMin)        or (WindowLog        > WindowLogMax))) or
    ((HashLog          <> 0) and ((HashLog          < HashLogMin)          or (HashLog          > HashLogMax))) or
    ((ChainLog         <> 0) and ((ChainLog         < ChainLogMin)         or (ChainLog         > ChainLogMax))) or
    ((SearchLog        <> 0) and ((SearchLog        < SearchLogMin)        or (SearchLog        > SearchLogMax))) or
    ((MinMatch         <> 0) and ((MinMatch         < MinMatchMin)         or (MinMatch         > MinMatchMax))) or
    ((TargetLength     <> 0) and ((TargetLength     < TargetLengthMin)     or (TargetLength     > TargetLengthMax))) or
    ((Strategy         <> 0) and ((Strategy         < StrategyMin)         or (Strategy         > StrategyMax))) or
    ((LdmHashLog       <> 0) and ((LdmHashLog       < LdmHashLogMin)       or (LdmHashLog       > LdmHashLogMax))) or
    ((LdmMinMatch      <> 0) and ((LdmMinMatch      < LdmMinMatchMin)      or (LdmMinMatch      > LdmMinMatchMax))) or
    ((LdmBucketSizeLog <> 0) and ((LdmBucketSizeLog < LdmBucketSizeLogMin) or (LdmBucketSizeLog > LdmBucketSizeLogMax))) or
    ((LdmHashRateLog   <> 0) and ((LdmHashRateLog   < LdmHashRateLogMin)   or (LdmHashRateLog   > LdmHashRateLogMax))) or
    ((Workers          <> 0) and ((Workers          < WorkersMin)          or (Workers          > WorkersMax))) or
    ((JobSize          <> 0) and ((JobSize          < JobSizeMin)          or (JobSize          > JobSizeMax))) or
    ((OverlapLog       <> 0) and ((OverlapLog       < OverlapLogMin)       or (OverlapLog       > OverlapLogMax)))
  then
    RaiseLastOSError(ERROR_INVALID_PARAMETER);
end;

//**************************************************************************************************
// TZSTDDecompressOptions
//**************************************************************************************************

procedure TZSTDDecompressOptions.Init;
begin
  ZeroMemory(@Self, SizeOf(Self));
end;

function TZSTDDecompressOptions.ParamMin(AParam: ZSTD_dParameter): Integer;
var
  Bounds: ZSTD_bounds;
begin
  Bounds := ZSTD_dParam_getBounds(AParam);
  ZSTDCheck(sZSTD_dParam_getBounds, Bounds.error);
  Result := Bounds.lowerBound;
end;

function TZSTDDecompressOptions.ParamMax(AParam: ZSTD_dParameter): Integer;
var
  Bounds: ZSTD_bounds;
begin
  Bounds := ZSTD_dParam_getBounds(AParam);
  ZSTDCheck(sZSTD_dParam_getBounds, Bounds.error);
  Result := Bounds.upperBound;
end;

function TZSTDDecompressOptions.WindowLogMin: Integer;
begin
  Result := ParamMin(ZSTD_d_windowLogMax);
end;

function TZSTDDecompressOptions.WindowLogMax: Integer;
begin
  Result := ParamMax(ZSTD_d_windowLogMax);
end;

procedure TZSTDDecompressOptions.Check;
begin
  if
    ((WindowLog <> 0) and ((WindowLog < WindowLogMin) or (WindowLog > WindowLogMax)))
  then
    RaiseLastOSError(ERROR_INVALID_PARAMETER);
end;

//**************************************************************************************************
// TZSTDCompressStream
//**************************************************************************************************

class function TZSTDCompressStream.MaxLevel: Integer;
begin
  Result := ZSTD_maxCLevel;
end;

constructor TZSTDCompressStream.Create(ADest: TStream; ALevel: Integer; AThreadCount: Integer = 0);
begin
  inherited Create;
  FOptions.Init;
  FDest := ADest;
  FOptions.CompressionLevel := ALevel;
  if FOptions.CompressionLevel < 1 then FOptions.CompressionLevel := 1;
  if FOptions.CompressionLevel > MaxLevel then FOptions.CompressionLevel := MaxLevel;
  FOptions.Workers := AThreadCount;
  if FOptions.Workers < 0 then
    FOptions.Workers := 0;
end;

constructor TZSTDCompressStream.Create(ADest: TStream; const AOption: TZSTDCompressOptions);
begin
  Create(ADest, AOption.CompressionLevel, AOption.Workers);
  FOptions := AOption;
end;

destructor TZSTDCompressStream.Destroy;
var
  Input: ZSTD_inBuffer;
  Output: ZSTD_outBuffer;
  R: size_t;
begin
  try
    if Assigned(FStream) then
      try
        Input.src := nil;
        Input.size := 0;
        Input.pos := 0;
        while True do
          begin
            Output.dst := FStreamOutBuffer;
            Output.size := FStreamOutBufferSize;
            Output.pos := 0;
            R := ZSTDCheck(sZSTD_compressStream2, ZSTD_compressStream2(FStream, Output, Input, ZSTD_e_end));
            if Output.pos > 0 then
              FDest.WriteBuffer(FStreamOutBuffer^, Output.pos);
            if R = 0 then Break;
          end;
      finally
        ZSTD_freeCStream(FStream);
      end;
  finally
    if Assigned(FStreamOutBuffer) then
      FreeMem(FStreamOutBuffer);
  end;
  inherited Destroy;
end;

function TZSTDCompressStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Result := 0; // Make compiler happy;
  RaiseLastOSError(ERROR_INVALID_FUNCTION);
end;

function TZSTDCompressStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  Result := 0; // Make compiler happy;
  RaiseLastOSError(ERROR_INVALID_FUNCTION);
end;

function TZSTDCompressStream.Write(const ABuffer; ACount: Longint): Longint;
const
  Bools: array[Boolean] of Integer = (0, 1);
var
  Input: ZSTD_inBuffer;
  Output: ZSTD_outBuffer;
begin
  Result := ACount;
  if ACount = 0 then Exit;

  if not Assigned(FStreamOutBuffer) then
    begin
      FStreamOutBufferSize := ZSTD_CStreamOutSize;
      GetMem(FStreamOutBuffer, FStreamOutBufferSize);
    end;

  if not Assigned(FStream) then
    begin
      FStream := ZSTD_createCStream;
      if not Assigned(FStream) then
        raise EOutOfMemory.Create('');
      //ZSTDCheck(sZSTD_initCStream, ZSTD_initCStream(FStream, FLevel));
      ZSTDCheck(sZSTD_CCtx_reset, ZSTD_CCtx_reset(FStream, ZSTD_reset_session_and_parameters));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_compressionLevel, FOptions.CompressionLevel));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_enableLongDistanceMatching, Bools[FOptions.EnableLongDistanceMatching]));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_windowLog, FOptions.WindowLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_hashLog, FOptions.HashLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_chainLog, FOptions.ChainLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_searchLog, FOptions.SearchLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_minMatch, FOptions.MinMatch));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_targetLength, FOptions.TargetLength));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_strategy, FOptions.Strategy));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmHashLog, FOptions.LdmHashLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmMinMatch, FOptions.LdmMinMatch));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmBucketSizeLog, FOptions.LdmBucketSizeLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmHashRateLog, FOptions.LdmHashRateLog));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_contentSizeFlag, Bools[FOptions.ContentSizeFlag]));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_checksumFlag, Bools[FOptions.ChecksumFlag]));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_dictIDFlag, Bools[FOptions.DictIDFlag]));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_nbWorkers, FOptions.Workers));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_jobSize, FOptions.JobSize));
      ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_overlapLog, FOptions.OverlapLog));
    end;

  Input.src := @ABuffer;
  Input.size := ACount;
  Input.pos := 0;

  while Input.pos < Input.size do
    begin
      Output.dst := FStreamOutBuffer;
      Output.size := FStreamOutBufferSize;
      Output.pos := 0;
      ZSTDCheck(sZSTD_compressStream2, ZSTD_compressStream2(FStream, Output, Input, ZSTD_e_continue));
      if Output.pos > 0 then
        FDest.WriteBuffer(FStreamOutBuffer^, Output.pos);
    end;
end;

//**************************************************************************************************
// TZSTDDecompressStream
//**************************************************************************************************

constructor TZSTDDecompressStream.Create(ASource: TStream);
begin
  inherited Create;
  FSource := ASource;
  FOptions.Init;
end;

constructor TZSTDDecompressStream.Create(ASource: TStream; const AOption: TZSTDDecompressOptions);
begin
  Create(ASource);
  FOptions := AOption;
end;

destructor TZSTDDecompressStream.Destroy;
begin
  if Assigned(FStream) then
    ZSTD_freeDStream(FStream);
  if Assigned(FStreamInBuffer) then
    FreeMem(FStreamInBuffer);
  if Assigned(FStreamOutBuffer) then
    FreeMem(FStreamOutBuffer);
  if Assigned(FSeekBuffer) then
    FreeMem(FSeekBuffer);
  inherited Destroy;
end;

function TZSTDDecompressStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
const
  SeekBufferSize = 65536;
var
  SeekSizeTotal: Int64;
  SeekSize: Integer;
begin
  if (AOrigin <> soCurrent) or (AOffset < 0) then
    begin
      Result := 0; // Make compiler happy;
      RaiseLastOSError(ERROR_INVALID_FUNCTION);
    end
  else
    begin
      if AOffset > 0 then
        begin
          if not Assigned(FSeekBuffer) then
            GetMem(FSeekBuffer, SeekBufferSize);
          SeekSizeTotal := AOffset;
          while SeekSizeTotal > 0 do
            begin
              if SeekSizeTotal >= SeekBufferSize then SeekSize := SeekBufferSize
                                                 else SeekSize := SeekSizeTotal;
              SeekSize := Read(FSeekBuffer^, SeekSize);
              if SeekSize = 0 then
                Break;
              Dec(SeekSizeTotal, SeekSize);
            end;
        end;
      Result := FPosition;
    end;
end;

function TZSTDDecompressStream.Read(var ABuffer; ACount: Longint): Longint;
var
  AvailableCount: size_t;
  Buffer: PByte;
  Source: PByte;
begin
  Result := 0;
  if ACount = 0 then Exit;

  if not Assigned(FStreamInBuffer) then
    begin
      FStreamInBufferSize := ZSTD_DStreamInSize;
      GetMem(FStreamInBuffer, FStreamInBufferSize);
      FInput.src := FStreamInBuffer;
      FInput.size := FStreamInBufferSize;
      FInput.pos := FStreamInBufferSize;
    end;

  if not Assigned(FStreamOutBuffer) then
    begin
      FStreamOutBufferSize := ZSTD_DStreamOutSize;
      GetMem(FStreamOutBuffer, FStreamOutBufferSize);
      FOutput.dst := FStreamOutBuffer;
      FOutput.size := FStreamOutBufferSize;
      FStreamOutBufferSizePos := 0;
    end;

  if not Assigned(FStream) then
    begin
      FStream := ZSTD_createDStream;
      if not Assigned(FStream) then
        raise EOutOfMemory.Create('');
      ZSTDCheck(sZSTD_initDStream, ZSTD_initDStream(FStream));
      ZSTDCheck(sZSTD_DCtx_setParameter, ZSTD_DCtx_setParameter(FStream, ZSTD_d_windowLogMax, FOptions.WindowLog));
    end;

  Buffer := @ABuffer;
  while ACount > 0 do
    begin
      AvailableCount := FOutput.pos - FStreamOutBufferSizePos;
      if Integer(AvailableCount) > ACount then
        AvailableCount := ACount;
      if AvailableCount > 0 then
        begin
          Source := FStreamOutBuffer;
          Inc(Source, FStreamOutBufferSizePos);
          CopyMemory(Buffer, Source, AvailableCount);
          Inc(FStreamOutBufferSizePos, AvailableCount);
          Inc(Buffer, AvailableCount);
          Dec(ACount, AvailableCount);
          Inc(Result, AvailableCount);
          Inc(FPosition, AvailableCount);
          if ACount = 0 then Break;
        end;

      FOutput.pos := 0;
      FStreamOutBufferSizePos := 0;

      if (FInput.pos = FInput.size) and (FInput.size > 0) then
        begin
          FInput.size := FSource.Read(FStreamInBuffer^, FInput.size);
          FInput.pos := 0;
        end;

      ZSTDCheck(sZSTD_compressStream, ZSTD_decompressStream(FStream, FOutput, FInput));
      if (FOutput.pos = 0) and (FInput.size = 0) then Break;
    end;
end;

function TZSTDDecompressStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  Result := 0; // Make compiler happy;
  RaiseLastOSError(ERROR_INVALID_FUNCTION);
end;

end.

