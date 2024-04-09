unit ZSTDLib;

(*
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under both the BSD-style license (found in the
 * LICENSE file in the root directory of this source tree) and the GPLv2 (found
 * in the COPYING file in the root directory of this source tree).
 * You may select, at your option, one of the above-listed licenses.
 *)

{.$DEFINE ZSTD_STATIC_LINKING}

{$IFDEF FPC}
 {$MODE DELPHI}
 {$PACKRECORDS C}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF FPC}
    DynLibs,
  {$ENDIF}
  SysUtils;

{$Z4}

{$IFDEF ZSTD_STATIC_LINKING}
const
  ZSTDDllName = 'libzstd.dll';
{$ELSE}
const
  ZSTDDllName = 'libzstd.dll';
{$ENDIF}

const
  sZSTD_versionNumber                       = 'ZSTD_versionNumber';
  sZSTD_versionString                       = 'ZSTD_versionString';
  sZSTD_compress                            = 'ZSTD_compress';
  sZSTD_decompress                          = 'ZSTD_decompress';
  sZSTD_getFrameContentSize                 = 'ZSTD_getFrameContentSize';
  sZSTD_getDecompressedSize                 = 'ZSTD_getDecompressedSize';
  sZSTD_findFrameCompressedSize             = 'ZSTD_findFrameCompressedSize';
  sZSTD_compressBound                       = 'ZSTD_compressBound';
  sZSTD_isError                             = 'ZSTD_isError';
  sZSTD_getErrorName                        = 'ZSTD_getErrorName';
  sZSTD_minCLevel                           = 'ZSTD_minCLevel';
  sZSTD_maxCLevel                           = 'ZSTD_maxCLevel';
  sZSTD_createCCtx                          = 'ZSTD_createCCtx';
  sZSTD_freeCCtx                            = 'ZSTD_freeCCtx';
  sZSTD_compressCCtx                        = 'ZSTD_compressCCtx';
  sZSTD_createDCtx                          = 'ZSTD_createDCtx';
  sZSTD_freeDCtx                            = 'ZSTD_freeDCtx';
  sZSTD_decompressDCtx                      = 'ZSTD_decompressDCtx';
  sZSTD_cParam_getBounds                    = 'ZSTD_cParam_getBounds';
  sZSTD_CCtx_setParameter                   = 'ZSTD_CCtx_setParameter';
  sZSTD_CCtx_setPledgedSrcSize              = 'ZSTD_CCtx_setPledgedSrcSize';
  sZSTD_CCtx_reset                          = 'ZSTD_CCtx_reset';
  sZSTD_compress2                           = 'ZSTD_compress2';
  sZSTD_dParam_getBounds                    = 'ZSTD_dParam_getBounds';
  sZSTD_DCtx_setParameter                   = 'ZSTD_DCtx_setParameter';
  sZSTD_DCtx_reset                          = 'ZSTD_DCtx_reset';
  sZSTD_createCStream                       = 'ZSTD_createCStream';
  sZSTD_freeCStream                         = 'ZSTD_freeCStream';
  sZSTD_compressStream2                     = 'ZSTD_compressStream2';
  sZSTD_CStreamInSize                       = 'ZSTD_CStreamInSize';
  sZSTD_CStreamOutSize                      = 'ZSTD_CStreamOutSize';
  sZSTD_initCStream                         = 'ZSTD_initCStream';
  sZSTD_compressStream                      = 'ZSTD_compressStream';
  sZSTD_flushStream                         = 'ZSTD_flushStream';
  sZSTD_endStream                           = 'ZSTD_endStream';
  sZSTD_createDStream                       = 'ZSTD_createDStream';
  sZSTD_freeDStream                         = 'ZSTD_freeDStream';
  sZSTD_initDStream                         = 'ZSTD_initDStream';
  sZSTD_decompressStream                    = 'ZSTD_decompressStream';
  sZSTD_DStreamInSize                       = 'ZSTD_DStreamInSize';
  sZSTD_DStreamOutSize                      = 'ZSTD_DStreamOutSize';
  sZSTD_compress_usingDict                  = 'ZSTD_compress_usingDict';
  sZSTD_decompress_usingDict                = 'ZSTD_decompress_usingDict';
  sZSTD_createCDict                         = 'ZSTD_createCDict';
  sZSTD_freeCDict                           = 'ZSTD_freeCDict';
  sZSTD_compress_usingCDict                 = 'ZSTD_compress_usingCDict';
  sZSTD_createDDict                         = 'ZSTD_createDDict';
  sZSTD_freeDDict                           = 'ZSTD_freeDDict';
  sZSTD_decompress_usingDDict               = 'ZSTD_decompress_usingDDict';
  sZSTD_getDictID_fromDict                  = 'ZSTD_getDictID_fromDict';
  sZSTD_getDictID_fromDDict                 = 'ZSTD_getDictID_fromDDict';
  sZSTD_getDictID_fromFrame                 = 'ZSTD_getDictID_fromFrame';
  sZSTD_CCtx_loadDictionary                 = 'ZSTD_CCtx_loadDictionary';
  sZSTD_CCtx_refCDict                       = 'ZSTD_CCtx_refCDict';
  sZSTD_CCtx_refPrefix                      = 'ZSTD_CCtx_refPrefix';
  sZSTD_DCtx_loadDictionary                 = 'ZSTD_DCtx_loadDictionary';
  sZSTD_DCtx_refDDict                       = 'ZSTD_DCtx_refDDict';
  sZSTD_DCtx_refPrefix                      = 'ZSTD_DCtx_refPrefix';
  sZSTD_sizeof_CCtx                         = 'ZSTD_sizeof_CCtx';
  sZSTD_sizeof_DCtx                         = 'ZSTD_sizeof_DCtx';
  sZSTD_sizeof_CStream                      = 'ZSTD_sizeof_CStream';
  sZSTD_sizeof_DStream                      = 'ZSTD_sizeof_DStream';
  sZSTD_sizeof_CDict                        = 'ZSTD_sizeof_CDict';
  sZSTD_sizeof_DDict                        = 'ZSTD_sizeof_DDict';

type
  EZSTDException = class(Exception)
  public
    constructor Create(const AFunctionName: string; ACode: NativeInt);
  private
    FCode: NativeInt
  end;

procedure ZSTDError(const AFunctionName: string; ACode: size_t);
function ZSTDCheck(const AFunctionName: string; ACode: size_t): size_t;

type
  unsigned = DWORD;
  int = Integer;

function ZSTD_versionNumber: unsigned; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_versionNumber;{$ENDIF}
function ZSTD_versionString: PAnsiChar; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_versionString;{$ENDIF}

(***************************************
*  Default constant
***************************************)

const
  ZSTD_CLEVEL_DEFAULT = 3;

(***************************************
*  Constants
***************************************)

(* All magic numbers are supposed read/written to/from files/memory using little-endian convention *)
  ZSTD_MAGICNUMBER           = $FD2FB528;    (* valid since v0.8.0 *)
  ZSTD_MAGIC_DICTIONARY      = $EC30A437;    (* valid since v0.7.0 *)
  ZSTD_MAGIC_SKIPPABLE_START = $184D2A50;    (* all 16 values, from 0x184D2A50 to 0x184D2A5F, signal the beginning of a skippable frame *)
  ZSTD_MAGIC_SKIPPABLE_MASK  = $FFFFFFF0;

  ZSTD_BLOCKSIZELOG_MAX = 17;
  ZSTD_BLOCKSIZE_MAX    = (1 shl ZSTD_BLOCKSIZELOG_MAX);

(***************************************
*  Simple API
***************************************)

(*! ZSTD_compress() :
 *  Compresses `src` content as a single zstd compressed frame into already allocated `dst`.
 *  Hint : compression runs faster if `dstCapacity` >=  `ZSTD_compressBound(srcSize)`.
 *  @return : compressed size written into `dst` (<= `dstCapacity),
 *            or an error code if it fails (which can be tested using ZSTD_isError()). *)
function ZSTD_compress(dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; compressionLevel: int): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compress;{$ENDIF}

(*! ZSTD_decompress() :
 *  `compressedSize` : must be the _exact_ size of some number of compressed and/or skippable frames.
 *  `dstCapacity` is an upper bound of originalSize to regenerate.
 *  If user cannot imply a maximum upper bound, it's better to use streaming mode to decompress data.
 *  @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
 *            or an errorCode if it fails (which can be tested using ZSTD_isError()). *)
function ZSTD_decompress(dst: Pointer; dstCapacity: size_t; src: Pointer; compressedSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_decompress;{$ENDIF}

(*! ZSTD_getFrameContentSize() : requires v1.3.0+
 *  `src` should point to the start of a ZSTD encoded frame.
 *  `srcSize` must be at least as large as the frame header.
 *            hint : any size >= `ZSTD_frameHeaderSize_max` is large enough.
 *  @return : - decompressed size of `src` frame content, if known
 *            - ZSTD_CONTENTSIZE_UNKNOWN if the size cannot be determined
 *            - ZSTD_CONTENTSIZE_ERROR if an error occurred (e.g. invalid magic number, srcSize too small)
 *   note 1 : a 0 return value means the frame is valid but "empty".
 *   note 2 : decompressed size is an optional field, it may not be present, typically in streaming mode.
 *            When `return==ZSTD_CONTENTSIZE_UNKNOWN`, data to decompress could be any size.
 *            In which case, it's necessary to use streaming mode to decompress data.
 *            Optionally, application can rely on some implicit limit,
 *            as ZSTD_decompress() only needs an upper bound of decompressed size.
 *            (For example, data could be necessarily cut into blocks <= 16 KB).
 *   note 3 : decompressed size is always present when compression is completed using single-pass functions,
 *            such as ZSTD_compress(), ZSTD_compressCCtx() ZSTD_compress_usingDict() or ZSTD_compress_usingCDict().
 *   note 4 : decompressed size can be very large (64-bits value),
 *            potentially larger than what local system can handle as a single memory segment.
 *            In which case, it's necessary to use streaming mode to decompress data.
 *   note 5 : If source is untrusted, decompressed size could be wrong or intentionally modified.
 *            Always ensure return value fits within application's authorized limits.
 *            Each application can set its own limits.
 *   note 6 : This function replaces ZSTD_getDecompressedSize() *)

const
  ZSTD_CONTENTSIZE_UNKNOWN = -1;
  ZSTD_CONTENTSIZE_ERROR   = -2;

function ZSTD_getFrameContentSize(src: Pointer; srcSize: size_t): Int64; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_getFrameContentSize;{$ENDIF}

(*! ZSTD_getDecompressedSize() :
 *  NOTE: This function is now obsolete, in favor of ZSTD_getFrameContentSize().
 *  Both functions work the same way, but ZSTD_getDecompressedSize() blends
 *  "empty", "unknown" and "error" results to the same return value (0),
 *  while ZSTD_getFrameContentSize() gives them separate return values.
 * @return : decompressed size of `src` frame content _if known and not empty_, 0 otherwise. *)
function ZSTD_getDecompressedSize(src: Pointer; srcSize: size_t): Int64; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_getDecompressedSize;{$ENDIF}

(*! ZSTD_findFrameCompressedSize() :
 * `src` should point to the start of a ZSTD frame or skippable frame.
 * `srcSize` must be >= first frame size
 * @return : the compressed size of the first frame starting at `src`,
 *           suitable to pass as `srcSize` to `ZSTD_decompress` or similar,
 *        or an error code if input is invalid *)
function ZSTD_findFrameCompressedSize(src: Pointer; srcSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_findFrameCompressedSize;{$ENDIF}

(*======  Helper functions  ======*)

function ZSTD_compressBound(srcSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compressBound;{$ENDIF}(*!< maximum compressed size in worst case single-pass scenario *)
function ZSTD_isError(code: size_t): unsigned; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_isError;{$ENDIF}       (*!< tells if a `size_t` function result is an error code *)
function ZSTD_getErrorName(code: size_t): PAnsiChar; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_getErrorName;{$ENDIF} (*!< provides readable string from an error code *)
function ZSTD_minCLevel: int; (*!< minimum negative compression level allowed *) {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_minCLevel;{$ENDIF}
function ZSTD_maxCLevel: int; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_maxCLevel;{$ENDIF}

(***************************************
*  Explicit context
***************************************)

(*= Compression context
 *  When compressing many times,
 *  it is recommended to allocate a context just once,
 *  and re-use it for each successive compression operation.
 *  This will make workload friendlier for system's memory.
 *  Note : re-using context is just a speed / resource optimization.
 *         It doesn't change the compression ratio, which remains identical.
 *  Note 2 : In multi-threaded environments,
 *         use one different context per thread for parallel execution.
 *)

type
  ZSTD_CCtx = type Pointer;

function ZSTD_createCCtx: ZSTD_CCtx; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_createCCtx;{$ENDIF}
function ZSTD_freeCCtx(cctx: ZSTD_CCtx): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_freeCCtx;{$ENDIF}

(*! ZSTD_compressCCtx() :
 *  Same as ZSTD_compress(), using an explicit ZSTD_CCtx
 *  The function will compress at requested compression level,
 *  ignoring any other parameter *)
function ZSTD_compressCCtx(ctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; compressionLevel: int): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compressCCtx;{$ENDIF}

(*= Decompression context
 *  When decompressing many times,
 *  it is recommended to allocate a context only once,
 *  and re-use it for each successive compression operation.
 *  This will make workload friendlier for system's memory.
 *  Use one context per thread for parallel execution. *)

type
  ZSTD_DCtx = type Pointer;

function ZSTD_createDCtx: ZSTD_DCtx; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_createDCtx;{$ENDIF}
function ZSTD_freeDCtx(dctx: ZSTD_DCtx): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_freeDCtx;{$ENDIF}

(*! ZSTD_decompressDCtx() :
 *  Same as ZSTD_decompress(),
 *  requires an allocated ZSTD_DCtx.
 *  Compatible with sticky parameters.
 *)
function ZSTD_decompressDCtx(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_decompressDCtx;{$ENDIF}

(***************************************
*  Advanced compression API
***************************************)

(* API design :
 *   Parameters are pushed one by one into an existing context,
 *   using ZSTD_CCtx_set*() functions.
 *   Pushed parameters are sticky : they are valid for next compressed frame, and any subsequent frame.
 *   "sticky" parameters are applicable to `ZSTD_compress2()` and `ZSTD_compressStream*()` !
 *   They do not apply to "simple" one-shot variants such as ZSTD_compressCCtx()
 *
 *   It's possible to reset all parameters to "default" using ZSTD_CCtx_reset().
 *
 *   This API supercedes all other "advanced" API entry points in the experimental section.
 *   In the future, we expect to remove from experimental API entry points which are redundant with this API.
 *)

(* Compression strategies, listed from fastest to strongest *)

type
  ZSTD_strategy = (
    ZSTD_fast     = 1,
    ZSTD_dfast    = 2,
    ZSTD_greedy   = 3,
    ZSTD_lazy     = 4,
    ZSTD_lazy2    = 5,
    ZSTD_btlazy2  = 6,
    ZSTD_btopt    = 7,
    ZSTD_btultra  = 8,
    ZSTD_btultra2 = 9
               (* note : new strategies _might_ be added in the future.
                         Only the order (from fast to strong) is guaranteed *)
    );


  ZSTD_cParameter = (
    (* compression parameters
     * Note: When compressing with a ZSTD_CDict these parameters are superseded
     * by the parameters used to construct the ZSTD_CDict. See ZSTD_CCtx_refCDict()
     * for more info (superseded-by-cdict). *)
    ZSTD_c_compressionLevel=100, (* Update all compression parameters according to pre-defined cLevel table
                              * Default level is ZSTD_CLEVEL_DEFAULT==3.
                              * Special: value 0 means default, which is controlled by ZSTD_CLEVEL_DEFAULT.
                              * Note 1 : it's possible to pass a negative compression level.
                              * Note 2 : setting a level sets all default values of other compression parameters *)
    ZSTD_c_windowLog=101,    (* Maximum allowed back-reference distance, expressed as power of 2.
                              * Must be clamped between ZSTD_WINDOWLOG_MIN and ZSTD_WINDOWLOG_MAX.
                              * Special: value 0 means "use default windowLog".
                              * Note: Using a windowLog greater than ZSTD_WINDOWLOG_LIMIT_DEFAULT
                              *       requires explicitly allowing such window size at decompression stage if using streaming. *)
    ZSTD_c_hashLog=102,      (* Size of the initial probe table, as a power of 2.
                              * Resulting memory usage is (1 << (hashLog+2)).
                              * Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX.
                              * Larger tables improve compression ratio of strategies <= dFast,
                              * and improve speed of strategies > dFast.
                              * Special: value 0 means "use default hashLog". *)
    ZSTD_c_chainLog=103,     (* Size of the multi-probe search table, as a power of 2.
                              * Resulting memory usage is (1 << (chainLog+2)).
                              * Must be clamped between ZSTD_CHAINLOG_MIN and ZSTD_CHAINLOG_MAX.
                              * Larger tables result in better and slower compression.
                              * This parameter is useless when using "fast" strategy.
                              * It's still useful when using "dfast" strategy,
                              * in which case it defines a secondary probe table.
                              * Special: value 0 means "use default chainLog". *)
    ZSTD_c_searchLog=104,    (* Number of search attempts, as a power of 2.
                              * More attempts result in better and slower compression.
                              * This parameter is useless when using "fast" and "dFast" strategies.
                              * Special: value 0 means "use default searchLog". *)
    ZSTD_c_minMatch=105,     (* Minimum size of searched matches.
                              * Note that Zstandard can still find matches of smaller size,
                              * it just tweaks its search algorithm to look for this size and larger.
                              * Larger values increase compression and decompression speed, but decrease ratio.
                              * Must be clamped between ZSTD_MINMATCH_MIN and ZSTD_MINMATCH_MAX.
                              * Note that currently, for all strategies < btopt, effective minimum is 4.
                              *                    , for all strategies > fast, effective maximum is 6.
                              * Special: value 0 means "use default minMatchLength". *)
    ZSTD_c_targetLength=106, (* Impact of this field depends on strategy.
                              * For strategies btopt, btultra & btultra2:
                              *     Length of Match considered "good enough" to stop search.
                              *     Larger values make compression stronger, and slower.
                              * For strategy fast:
                              *     Distance between match sampling.
                              *     Larger values make compression faster, and weaker.
                              * Special: value 0 means "use default targetLength". *)
    ZSTD_c_strategy=107,     (* See ZSTD_strategy enum definition.
                              * The higher the value of selected strategy, the more complex it is,
                              * resulting in stronger and slower compression.
                              * Special: value 0 means "use default strategy". *)

    (* LDM mode parameters *)
    ZSTD_c_enableLongDistanceMatching=160, (* Enable long distance matching.
                                     * This parameter is designed to improve compression ratio
                                     * for large inputs, by finding large matches at long distance.
                                     * It increases memory usage and window size.
                                     * Note: enabling this parameter increases default ZSTD_c_windowLog to 128 MB
                                     * except when expressly set to a different value. *)
    ZSTD_c_ldmHashLog=161,   (* Size of the table for long distance matching, as a power of 2.
                              * Larger values increase memory usage and compression ratio,
                              * but decrease compression speed.
                              * Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX
                              * default: windowlog - 7.
                              * Special: value 0 means "automatically determine hashlog". *)
    ZSTD_c_ldmMinMatch=162,  (* Minimum match size for long distance matcher.
                              * Larger/too small values usually decrease compression ratio.
                              * Must be clamped between ZSTD_LDM_MINMATCH_MIN and ZSTD_LDM_MINMATCH_MAX.
                              * Special: value 0 means "use default value" (default: 64). *)
    ZSTD_c_ldmBucketSizeLog=163, (* Log size of each bucket in the LDM hash table for collision resolution.
                              * Larger values improve collision resolution but decrease compression speed.
                              * The maximum value is ZSTD_LDM_BUCKETSIZELOG_MAX.
                              * Special: value 0 means "use default value" (default: 3). *)
    ZSTD_c_ldmHashRateLog=164, (* Frequency of inserting/looking up entries into the LDM hash table.
                              * Must be clamped between 0 and (ZSTD_WINDOWLOG_MAX - ZSTD_HASHLOG_MIN).
                              * Default is MAX(0, (windowLog - ldmHashLog)), optimizing hash table usage.
                              * Larger values improve compression speed.
                              * Deviating far from default value will likely result in a compression ratio decrease.
                              * Special: value 0 means "automatically determine hashRateLog". *)

    (* frame parameters *)
    ZSTD_c_contentSizeFlag=200, (* Content size will be written into frame header _whenever known_ (default:1)
                              * Content size must be known at the beginning of compression.
                              * This is automatically the case when using ZSTD_compress2(),
                              * For streaming variants, content size must be provided with ZSTD_CCtx_setPledgedSrcSize() *)
    ZSTD_c_checksumFlag=201, (* A 32-bits checksum of content is written at end of frame (default:0) *)
    ZSTD_c_dictIDFlag=202,   (* When applicable, dictionary's ID is written into frame header (default:1) *)

    (* multi-threading parameters *)
    (* These parameters are only useful if multi-threading is enabled (compiled with build macro ZSTD_MULTITHREAD).
     * They return an error otherwise. *)
    ZSTD_c_nbWorkers=400,    (* Select how many threads will be spawned to compress in parallel.
                              * When nbWorkers >= 1, triggers asynchronous mode when used with ZSTD_compressStream*() :
                              * ZSTD_compressStream*() consumes input and flush output if possible, but immediately gives back control to caller,
                              * while compression work is performed in parallel, within worker threads.
                              * (note : a strong exception to this rule is when first invocation of ZSTD_compressStream2() sets ZSTD_e_end :
                              *  in which case, ZSTD_compressStream2() delegates to ZSTD_compress2(), which is always a blocking call).
                              * More workers improve speed, but also increase memory usage.
                              * Default value is `0`, aka "single-threaded mode" : no worker is spawned, compression is performed inside Caller's thread, all invocations are blocking *)
    ZSTD_c_jobSize=401,      (* Size of a compression job. This value is enforced only when nbWorkers >= 1.
                              * Each compression job is completed in parallel, so this value can indirectly impact the nb of active threads.
                              * 0 means default, which is dynamically determined based on compression parameters.
                              * Job size must be a minimum of overlap size, or 1 MB, whichever is largest.
                              * The minimum size is automatically and transparently enforced *)
    ZSTD_c_overlapLog=402,   (* Control the overlap size, as a fraction of window size.
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

    (* note : additional experimental parameters are also available
     * within the experimental section of the API.
     * At the time of this writing, they include :
     * ZSTD_c_rsyncable
     * ZSTD_c_format
     * ZSTD_c_forceMaxWindow
     * ZSTD_c_forceAttachDict
     * ZSTD_c_literalCompressionMode
     * ZSTD_c_targetCBlockSize
     * Because they are not stable, it's necessary to define ZSTD_STATIC_LINKING_ONLY to access them.
     * note : never ever use experimentalParam? names directly;
     *        also, the enums values themselves are unstable and can still change.
     *)
     ZSTD_c_experimentalParam1=500,
     ZSTD_c_experimentalParam2=10,
     ZSTD_c_experimentalParam3=1000,
     ZSTD_c_experimentalParam4=1001,
     ZSTD_c_experimentalParam5=1002,
     ZSTD_c_experimentalParam6=1003
  );


  ZSTD_bounds = record
    error: size_t;
    lowerBound: int;
    upperBound: int;
  end;

(*! ZSTD_cParam_getBounds() :
 *  All parameters must belong to an interval with lower and upper bounds,
 *  otherwise they will either trigger an error or be automatically clamped.
 * @return : a structure, ZSTD_bounds, which contains
 *         - an error status field, which must be tested using ZSTD_isError()
 *         - lower and upper bounds, both inclusive
 *)
function ZSTD_cParam_getBounds(cParam: ZSTD_cParameter): ZSTD_bounds; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_cParam_getBounds;{$ENDIF}

(*! ZSTD_CCtx_setParameter() :
 *  Set one compression parameter, selected by enum ZSTD_cParameter.
 *  All parameters have valid bounds. Bounds can be queried using ZSTD_cParam_getBounds().
 *  Providing a value beyond bound will either clamp it, or trigger an error (depending on parameter).
 *  Setting a parameter is generally only possible during frame initialization (before starting compression).
 *  Exception : when using multi-threading mode (nbWorkers >= 1),
 *              the following parameters can be updated _during_ compression (within same frame):
 *              => compressionLevel, hashLog, chainLog, searchLog, minMatch, targetLength and strategy.
 *              new parameters will be active for next job only (after a flush()).
 * @return : an error code (which can be tested using ZSTD_isError()).
 *)
function ZSTD_CCtx_setParameter(cctx: ZSTD_CCtx; param: ZSTD_cParameter; value: int): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_setParameter;{$ENDIF}

(*! ZSTD_CCtx_setPledgedSrcSize() :
 *  Total input data size to be compressed as a single frame.
 *  Value will be written in frame header, unless if explicitly forbidden using ZSTD_c_contentSizeFlag.
 *  This value will also be controlled at end of frame, and trigger an error if not respected.
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Note 1 : pledgedSrcSize==0 actually means zero, aka an empty frame.
 *           In order to mean "unknown content size", pass constant ZSTD_CONTENTSIZE_UNKNOWN.
 *           ZSTD_CONTENTSIZE_UNKNOWN is default value for any new frame.
 *  Note 2 : pledgedSrcSize is only valid once, for the next frame.
 *           It's discarded at the end of the frame, and replaced by ZSTD_CONTENTSIZE_UNKNOWN.
 *  Note 3 : Whenever all input data is provided and consumed in a single round,
 *           for example with ZSTD_compress2(),
 *           or invoking immediately ZSTD_compressStream2(,,,ZSTD_e_end),
 *           this value is automatically overridden by srcSize instead.
 *)
function ZSTD_CCtx_setPledgedSrcSize(cctx: ZSTD_CCtx; pledgedSrcSize: Int64): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_setPledgedSrcSize;{$ENDIF}

type
  ZSTD_ResetDirective = (
    ZSTD_reset_session_only           = 1,
    ZSTD_reset_parameters             = 2,
    ZSTD_reset_session_and_parameters = 3
  );

(*! ZSTD_CCtx_reset() :
 *  There are 2 different things that can be reset, independently or jointly :
 *  - The session : will stop compressing current frame, and make CCtx ready to start a new one.
 *                  Useful after an error, or to interrupt any ongoing compression.
 *                  Any internal data not yet flushed is cancelled.
 *                  Compression parameters and dictionary remain unchanged.
 *                  They will be used to compress next frame.
 *                  Resetting session never fails.
 *  - The parameters : changes all parameters back to "default".
 *                  This removes any reference to any dictionary too.
 *                  Parameters can only be changed between 2 sessions (i.e. no compression is currently ongoing)
 *                  otherwise the reset fails, and function returns an error value (which can be tested using ZSTD_isError())
 *  - Both : similar to resetting the session, followed by resetting parameters.
 *)
function ZSTD_CCtx_reset(cctx: ZSTD_CCtx; reset: ZSTD_ResetDirective): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_reset;{$ENDIF}

(*! ZSTD_compress2() :
 *  Behave the same as ZSTD_compressCCtx(), but compression parameters are set using the advanced API.
 *  ZSTD_compress2() always starts a new frame.
 *  Should cctx hold data from a previously unfinished frame, everything about it is forgotten.
 *  - Compression parameters are pushed into CCtx before starting compression, using ZSTD_CCtx_set*()
 *  - The function is always blocking, returns when compression is completed.
 *  Hint : compression runs faster if `dstCapacity` >=  `ZSTD_compressBound(srcSize)`.
 * @return : compressed size written into `dst` (<= `dstCapacity),
 *           or an error code if it fails (which can be tested using ZSTD_isError()).
 *)
function ZSTD_compress2(cctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_reset;{$ENDIF}

(***************************************
*  Advanced decompression API
***************************************)

(* The advanced API pushes parameters one by one into an existing DCtx context.
 * Parameters are sticky, and remain valid for all following frames
 * using the same DCtx context.
 * It's possible to reset parameters to default values using ZSTD_DCtx_reset().
 * Note : This API is compatible with existing ZSTD_decompressDCtx() and ZSTD_decompressStream().
 *        Therefore, no new decompression function is necessary.
 *)

type
  ZSTD_dParameter = (
    ZSTD_d_windowLogMax=100, (* Select a size limit (in power of 2) beyond which
                              * the streaming API will refuse to allocate memory buffer
                              * in order to protect the host from unreasonable memory requirements.
                              * This parameter is only useful in streaming mode, since no internal buffer is allocated in single-pass mode.
                              * By default, a decompression context accepts window sizes <= (1 << ZSTD_WINDOWLOG_LIMIT_DEFAULT).
                              * Special: value 0 means "use default maximum windowLog". *)

    (* note : additional experimental parameters are also available
     * within the experimental section of the API.
     * At the time of this writing, they include :
     * ZSTD_c_format
     * Because they are not stable, it's necessary to define ZSTD_STATIC_LINKING_ONLY to access them.
     * note : never ever use experimentalParam? names directly
     *)
     ZSTD_d_experimentalParam1=1000
  );

(*! ZSTD_dParam_getBounds() :
 *  All parameters must belong to an interval with lower and upper bounds,
 *  otherwise they will either trigger an error or be automatically clamped.
 * @return : a structure, ZSTD_bounds, which contains
 *         - an error status field, which must be tested using ZSTD_isError()
 *         - both lower and upper bounds, inclusive
 *)
function ZSTD_dParam_getBounds(dParam: ZSTD_dParameter): ZSTD_bounds; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_dParam_getBounds;{$ENDIF}

(*! ZSTD_DCtx_setParameter() :
 *  Set one compression parameter, selected by enum ZSTD_dParameter.
 *  All parameters have valid bounds. Bounds can be queried using ZSTD_dParam_getBounds().
 *  Providing a value beyond bound will either clamp it, or trigger an error (depending on parameter).
 *  Setting a parameter is only possible during frame initialization (before starting decompression).
 * @return : 0, or an error code (which can be tested using ZSTD_isError()).
 *)
function ZSTD_DCtx_setParameter(dctx: ZSTD_DCtx; param: ZSTD_dParameter; value: int): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DCtx_setParameter;{$ENDIF}

(*! ZSTD_DCtx_reset() :
 *  Return a DCtx to clean state.
 *  Session and parameters can be reset jointly or separately.
 *  Parameters can only be reset when no active frame is being decompressed.
 * @return : 0, or an error code, which can be tested with ZSTD_isError()
 *)
function ZSTD_DCtx_reset(dctx: ZSTD_DCtx; reset: ZSTD_ResetDirective): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DCtx_reset;{$ENDIF}

(****************************
*  Streaming
****************************)

type
  ZSTD_inBuffer = record
    src: Pointer;    (**< start of input buffer *)
    size: size_t;    (**< size of input buffer *)
    pos: size_t;     (**< position where reading stopped. Will be updated. Necessarily 0 <= pos <= size *)
  end;

  ZSTD_outBuffer = record
    dst: Pointer;    (**< start of output buffer *)
    size: size_t;    (**< size of output buffer *)
    pos: size_t;     (**< position where writing stopped. Will be updated. Necessarily 0 <= pos <= size *)
  end;

(*-***********************************************************************
*  Streaming compression - HowTo
*
*  A ZSTD_CStream object is required to track streaming operation.
*  Use ZSTD_createCStream() and ZSTD_freeCStream() to create/release resources.
*  ZSTD_CStream objects can be reused multiple times on consecutive compression operations.
*  It is recommended to re-use ZSTD_CStream since it will play nicer with system's memory, by re-using already allocated memory.
*
*  For parallel execution, use one separate ZSTD_CStream per thread.
*
*  note : since v1.3.0, ZSTD_CStream and ZSTD_CCtx are the same thing.
*
*  Parameters are sticky : when starting a new compression on the same context,
*  it will re-use the same sticky parameters as previous compression session.
*  When in doubt, it's recommended to fully initialize the context before usage.
*  Use ZSTD_CCtx_reset() to reset the context and ZSTD_CCtx_setParameter(),
*  ZSTD_CCtx_setPledgedSrcSize(), or ZSTD_CCtx_loadDictionary() and friends to
*  set more specific parameters, the pledged source size, or load a dictionary.
*
*  Use ZSTD_compressStream2() with ZSTD_e_continue as many times as necessary to
*  consume input stream. The function will automatically update both `pos`
*  fields within `input` and `output`.
*  Note that the function may not consume the entire input, for example, because
*  the output buffer is already full, in which case `input.pos < input.size`.
*  The caller must check if input has been entirely consumed.
*  If not, the caller must make some room to receive more compressed data,
*  and then present again remaining input data.
*  note: ZSTD_e_continue is guaranteed to make some forward progress when called,
*        but doesn't guarantee maximal forward progress. This is especially relevant
*        when compressing with multiple threads. The call won't block if it can
*        consume some input, but if it can't it will wait for some, but not all,
*        output to be flushed.
* @return : provides a minimum amount of data remaining to be flushed from internal buffers
*           or an error code, which can be tested using ZSTD_isError().
*
*  At any moment, it's possible to flush whatever data might remain stuck within internal buffer,
*  using ZSTD_compressStream2() with ZSTD_e_flush. `output->pos` will be updated.
*  Note that, if `output->size` is too small, a single invocation with ZSTD_e_flush might not be enough (return code > 0).
*  In which case, make some room to receive more compressed data, and call again ZSTD_compressStream2() with ZSTD_e_flush.
*  You must continue calling ZSTD_compressStream2() with ZSTD_e_flush until it returns 0, at which point you can change the
*  operation.
*  note: ZSTD_e_flush will flush as much output as possible, meaning when compressing with multiple threads, it will
*        block until the flush is complete or the output buffer is full.
*  @return : 0 if internal buffers are entirely flushed,
*            >0 if some data still present within internal buffer (the value is minimal estimation of remaining size),
*            or an error code, which can be tested using ZSTD_isError().
*
*  Calling ZSTD_compressStream2() with ZSTD_e_end instructs to finish a frame.
*  It will perform a flush and write frame epilogue.
*  The epilogue is required for decoders to consider a frame completed.
*  flush operation is the same, and follows same rules as calling ZSTD_compressStream2() with ZSTD_e_flush.
*  You must continue calling ZSTD_compressStream2() with ZSTD_e_end until it returns 0, at which point you are free to
*  start a new frame.
*  note: ZSTD_e_end will flush as much output as possible, meaning when compressing with multiple threads, it will
*        block until the flush is complete or the output buffer is full.
*  @return : 0 if frame fully completed and fully flushed,
*            >0 if some data still present within internal buffer (the value is minimal estimation of remaining size),
*            or an error code, which can be tested using ZSTD_isError().
* *******************************************************************)

type
  ZSTD_CStream = type ZSTD_CCtx;

(*===== ZSTD_CStream management functions =====*)
function ZSTD_createCStream: ZSTD_CStream; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_createCStream;{$ENDIF}
function ZSTD_freeCStream(zcs: ZSTD_CStream): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_freeCStream;{$ENDIF}

(*===== Streaming compression functions =====*)
type
  ZSTD_EndDirective = (
    ZSTD_e_continue=0, (* collect more data, encoder decides when to output compressed result, for optimal compression ratio *)
    ZSTD_e_flush=1,    (* flush any data provided so far,
                        * it creates (at least) one new block, that can be decoded immediately on reception;
                        * frame will continue: any future data can still reference previously compressed data, improving compression.
                        * note : multithreaded compression will block to flush as much output as possible. *)
    ZSTD_e_end=2       (* flush any remaining data _and_ close current frame.
                        * note that frame is only closed after compressed data is fully flushed (return value == 0).
                        * After that point, any additional data starts a new frame.
                        * note : each frame is independent (does not reference any content from previous frame).
                        : note : multithreaded compression will block to flush as much output as possible. *)
  );

(*! ZSTD_compressStream2() :
 *  Behaves about the same as ZSTD_compressStream, with additional control on end directive.
 *  - Compression parameters are pushed into CCtx before starting compression, using ZSTD_CCtx_set*()
 *  - Compression parameters cannot be changed once compression is started (save a list of exceptions in multi-threading mode)
 *  - output->pos must be <= dstCapacity, input->pos must be <= srcSize
 *  - output->pos and input->pos will be updated. They are guaranteed to remain below their respective limit.
 *  - When nbWorkers==0 (default), function is blocking : it completes its job before returning to caller.
 *  - When nbWorkers>=1, function is non-blocking : it just acquires a copy of input, and distributes jobs to internal worker threads, flush whatever is available,
 *                                                  and then immediately returns, just indicating that there is some data remaining to be flushed.
 *                                                  The function nonetheless guarantees forward progress : it will return only after it reads or write at least 1+ byte.
 *  - Exception : if the first call requests a ZSTD_e_end directive and provides enough dstCapacity, the function delegates to ZSTD_compress2() which is always blocking.
 *  - @return provides a minimum amount of data remaining to be flushed from internal buffers
 *            or an error code, which can be tested using ZSTD_isError().
 *            if @return != 0, flush is not fully completed, there is still some data left within internal buffers.
 *            This is useful for ZSTD_e_flush, since in this case more flushes are necessary to empty all buffers.
 *            For ZSTD_e_end, @return == 0 when internal buffers are fully flushed and frame is completed.
 *  - after a ZSTD_e_end directive, if internal buffer is not fully flushed (@return != 0),
 *            only ZSTD_e_end or ZSTD_e_flush operations are allowed.
 *            Before starting a new compression job, or changing compression parameters,
 *            it is required to fully flush internal buffers.
 *)
function ZSTD_compressStream2(cctx: ZSTD_CCtx; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer; endOp: ZSTD_EndDirective): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compressStream2;{$ENDIF}

(* These buffer sizes are softly recommended.
 * They are not required : ZSTD_compressStream*() happily accepts any buffer size, for both input and output.
 * Respecting the recommended size just makes it a bit easier for ZSTD_compressStream*(),
 * reducing the amount of memory shuffling and buffering, resulting in minor performance savings.
 *
 * However, note that these recommendations are from the perspective of a C caller program.
 * If the streaming interface is invoked from some other language,
 * especially managed ones such as Java or Go, through a foreign function interface such as jni or cgo,
 * a major performance rule is to reduce crossing such interface to an absolute minimum.
 * It's not rare that performance ends being spent more into the interface, rather than compression itself.
 * In which cases, prefer using large buffers, as large as practical,
 * for both input and output, to reduce the nb of roundtrips.
 *)

function ZSTD_CStreamInSize: size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CStreamInSize;{$ENDIF} (**< recommended size for input buffer *)
function ZSTD_CStreamOutSize: size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CStreamOutSize;{$ENDIF} (**< recommended size for output buffer. Guarantee to successfully flush at least one complete compressed block in all circumstances. *)

(* *****************************************************************************
 * This following is a legacy streaming API.
 * It can be replaced by ZSTD_CCtx_reset() and ZSTD_compressStream2().
 * It is redundant, but remains fully supported.
 * Advanced parameters and dictionary compression can only be used through the
 * new API.
 ******************************************************************************)

(**
 * Equivalent to:
 *
 *     ZSTD_CCtx_reset(zcs, ZSTD_reset_session_only);
 *     ZSTD_CCtx_refCDict(zcs, NULL); // clear the dictionary (if any)
 *     ZSTD_CCtx_setParameter(zcs, ZSTD_c_compressionLevel, compressionLevel);
 *)
function ZSTD_initCStream(zcs: ZSTD_CStream; compressionLevel: int): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_initCStream;{$ENDIF}
(**
 * Alternative for ZSTD_compressStream2(zcs, output, input, ZSTD_e_continue).
 * NOTE: The return value is different. ZSTD_compressStream() returns a hint for
 * the next read size (if non-zero and not an error). ZSTD_compressStream2()
 * returns the minimum nb of bytes left to flush (if non-zero and not an error).
 *)
function ZSTD_compressStream(zcs: ZSTD_CStream; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compressStream;{$ENDIF}
(** Equivalent to ZSTD_compressStream2(zcs, output, &emptyInput, ZSTD_e_flush). *)
function ZSTD_flushStream(zcs: ZSTD_CStream; var output: ZSTD_outBuffer): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_flushStream;{$ENDIF}
(** Equivalent to ZSTD_compressStream2(zcs, output, &emptyInput, ZSTD_e_end). *)
function ZSTD_endStream(zcs: ZSTD_CStream; var output: ZSTD_outBuffer): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_endStream;{$ENDIF}

(*-***************************************************************************
*  Streaming decompression - HowTo
*
*  A ZSTD_DStream object is required to track streaming operations.
*  Use ZSTD_createDStream() and ZSTD_freeDStream() to create/release resources.
*  ZSTD_DStream objects can be re-used multiple times.
*
*  Use ZSTD_initDStream() to start a new decompression operation.
* @return : recommended first input size
*  Alternatively, use advanced API to set specific properties.
*
*  Use ZSTD_decompressStream() repetitively to consume your input.
*  The function will update both `pos` fields.
*  If `input.pos < input.size`, some input has not been consumed.
*  It's up to the caller to present again remaining data.
*  The function tries to flush all data decoded immediately, respecting output buffer size.
*  If `output.pos < output.size`, decoder has flushed everything it could.
*  But if `output.pos == output.size`, there might be some data left within internal buffers.,
*  In which case, call ZSTD_decompressStream() again to flush whatever remains in the buffer.
*  Note : with no additional input provided, amount of data flushed is necessarily <= ZSTD_BLOCKSIZE_MAX.
* @return : 0 when a frame is completely decoded and fully flushed,
*        or an error code, which can be tested using ZSTD_isError(),
*        or any other value > 0, which means there is still some decoding or flushing to do to complete current frame :
*                                the return value is a suggested next input size (just a hint for better latency)
*                                that will never request more than the remaining frame size.
* *******************************************************************************)

type
  ZSTD_DStream = type ZSTD_DCtx;

(*===== ZSTD_DStream management functions =====*)
function ZSTD_createDStream: ZSTD_DStream; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_createDStream;{$ENDIF}
function ZSTD_freeDStream(zds: ZSTD_DStream): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_freeDStream;{$ENDIF}

(*===== Streaming decompression functions =====*)

(* This function is redundant with the advanced API and equivalent to:
 *
 *     ZSTD_DCtx_reset(zds);
 *     ZSTD_DCtx_refDDict(zds, NULL);
 *)
function ZSTD_initDStream(zds: ZSTD_DStream): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_initDStream;{$ENDIF}
function ZSTD_decompressStream(zds: ZSTD_DStream; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_decompressStream;{$ENDIF}

function ZSTD_DStreamInSize: size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DStreamInSize;{$ENDIF} (*!< recommended size for input buffer *)
function ZSTD_DStreamOutSize: size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DStreamOutSize;{$ENDIF} (*!< recommended size for output buffer. Guarantee to successfully flush at least one complete block in all circumstances. *)

(**************************
*  Simple dictionary API
***************************)

(*! ZSTD_compress_usingDict() :
 *  Compression at an explicit compression level using a Dictionary.
 *  A dictionary can be any arbitrary data segment (also called a prefix),
 *  or a buffer with specified information (see dictBuilder/zdict.h).
 *  Note : This function loads the dictionary, resulting in significant startup delay.
 *         It's intended for a dictionary used only once.
 *  Note 2 : When `dict == NULL || dictSize < 8` no dictionary is used. *)
function ZSTD_compress_usingDict(ctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; dict: Pointer; dictSize: size_t; compressionLevel: int): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compress_usingDict;{$ENDIF}

(*! ZSTD_decompress_usingDict() :
 *  Decompression using a known Dictionary.
 *  Dictionary must be identical to the one used during compression.
 *  Note : This function loads the dictionary, resulting in significant startup delay.
 *         It's intended for a dictionary used only once.
 *  Note : When `dict == NULL || dictSize < 8` no dictionary is used. *)
function ZSTD_decompress_usingDict(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; dict: Pointer; dictSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_decompress_usingDict;{$ENDIF}

(**********************************
 *  Bulk processing dictionary API
 *********************************)

type
  ZSTD_CDict = type Pointer;

(*! ZSTD_createCDict() :
 *  When compressing multiple messages / blocks using the same dictionary, it's recommended to load it only once.
 *  ZSTD_createCDict() will create a digested dictionary, ready to start future compression operations without startup cost.
 *  ZSTD_CDict can be created once and shared by multiple threads concurrently, since its usage is read-only.
 * `dictBuffer` can be released after ZSTD_CDict creation, because its content is copied within CDict.
 *  Consider experimental function `ZSTD_createCDict_byReference()` if you prefer to not duplicate `dictBuffer` content.
 *  Note : A ZSTD_CDict can be created from an empty dictBuffer, but it is inefficient when used to compress small data. *)
function ZSTD_createCDict(dictBuffer: Pointer; dictSize: size_t; compressionLevel: int): ZSTD_CDict; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_createCDict;{$ENDIF}

(*! ZSTD_freeCDict() :
 *  Function frees memory allocated by ZSTD_createCDict(). *)
function ZSTD_freeCDict(CDict: ZSTD_CDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_freeCDict;{$ENDIF}

(*! ZSTD_compress_usingCDict() :
 *  Compression using a digested Dictionary.
 *  Recommended when same dictionary is used multiple times.
 *  Note : compression level is _decided at dictionary creation time_,
 *     and frame parameters are hardcoded (dictID=yes, contentSize=yes, checksum=no) *)
function ZSTD_compress_usingCDict(cctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; cdict: ZSTD_CDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_compress_usingCDict;{$ENDIF}

type
  ZSTD_DDict = type Pointer;

(*! ZSTD_createDDict() :
 *  Create a digested dictionary, ready to start decompression operation without startup delay.
 *  dictBuffer can be released after DDict creation, as its content is copied inside DDict. *)
function ZSTD_createDDict(dictBuffer: Pointer; dictSize: size_t): ZSTD_DDict; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_createDDict;{$ENDIF}

(*! ZSTD_freeDDict() :
 *  Function frees memory allocated with ZSTD_createDDict() *)
function ZSTD_freeDDict(ddict: ZSTD_DDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_freeDDict;{$ENDIF}

(*! ZSTD_decompress_usingDDict() :
 *  Decompression using a digested Dictionary.
 *  Recommended when same dictionary is used multiple times. *)
function ZSTD_decompress_usingDDict(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; ddict: ZSTD_DDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_decompress_usingDDict;{$ENDIF}

(********************************
 *  Dictionary helper functions
 *******************************)

(*! ZSTD_getDictID_fromDict() :
 *  Provides the dictID stored within dictionary.
 *  if @return == 0, the dictionary is not conformant with Zstandard specification.
 *  It can still be loaded, but as a content-only dictionary. *)
function ZSTD_getDictID_fromDict(dict: Pointer; dictSize: size_t): unsigned; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_getDictID_fromDict;{$ENDIF}

(*! ZSTD_getDictID_fromDDict() :
 *  Provides the dictID of the dictionary loaded into `ddict`.
 *  If @return == 0, the dictionary is not conformant to Zstandard specification, or empty.
 *  Non-conformant dictionaries can still be loaded, but as content-only dictionaries. *)
function ZSTD_getDictID_fromDDict(ddict: ZSTD_DDict): unsigned; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_getDictID_fromDDict;{$ENDIF}

(*! ZSTD_getDictID_fromFrame() :
 *  Provides the dictID required to decompressed the frame stored within `src`.
 *  If @return == 0, the dictID could not be decoded.
 *  This could for one of the following reasons :
 *  - The frame does not require a dictionary to be decoded (most common case).
 *  - The frame was built with dictID intentionally removed. Whatever dictionary is necessary is a hidden information.
 *    Note : this use case also happens when using a non-conformant dictionary.
 *  - `srcSize` is too small, and as a result, the frame header could not be decoded (only possible if `srcSize < ZSTD_FRAMEHEADERSIZE_MAX`).
 *  - This is not a Zstandard frame.
 *  When identifying the exact failure cause, it's possible to use ZSTD_getFrameHeader(), which will provide a more precise error code. *)
function ZSTD_getDictID_fromFrame(src: Pointer; srcSize: size_t): unsigned; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_getDictID_fromFrame;{$ENDIF}

(*******************************************************************************
 * Advanced dictionary and prefix API
 *
 * This API allows dictionaries to be used with ZSTD_compress2(),
 * ZSTD_compressStream2(), and ZSTD_decompress(). Dictionaries are sticky, and
 * only reset with the context is reset with ZSTD_reset_parameters or
 * ZSTD_reset_session_and_parameters. Prefixes are single-use.
 ******************************************************************************)

(*! ZSTD_CCtx_loadDictionary() :
 *  Create an internal CDict from `dict` buffer.
 *  Decompression will have to use same dictionary.
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Special: Loading a NULL (or 0-size) dictionary invalidates previous dictionary,
 *           meaning "return to no-dictionary mode".
 *  Note 1 : Dictionary is sticky, it will be used for all future compressed frames.
 *           To return to "no-dictionary" situation, load a NULL dictionary (or reset parameters).
 *  Note 2 : Loading a dictionary involves building tables.
 *           It's also a CPU consuming operation, with non-negligible impact on latency.
 *           Tables are dependent on compression parameters, and for this reason,
 *           compression parameters can no longer be changed after loading a dictionary.
 *  Note 3 :`dict` content will be copied internally.
 *           Use experimental ZSTD_CCtx_loadDictionary_byReference() to reference content instead.
 *           In such a case, dictionary buffer must outlive its users.
 *  Note 4 : Use ZSTD_CCtx_loadDictionary_advanced()
 *           to precisely select how dictionary content must be interpreted. *)
function ZSTD_CCtx_loadDictionary(cctx: ZSTD_CCtx; dict: Pointer; dictSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_loadDictionary;{$ENDIF}

(*! ZSTD_CCtx_refCDict() :
 *  Reference a prepared dictionary, to be used for all next compressed frames.
 *  Note that compression parameters are enforced from within CDict,
 *  and supersede any compression parameter previously set within CCtx.
 *  The parameters ignored are labled as "superseded-by-cdict" in the ZSTD_cParameter enum docs.
 *  The ignored parameters will be used again if the CCtx is returned to no-dictionary mode.
 *  The dictionary will remain valid for future compressed frames using same CCtx.
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Special : Referencing a NULL CDict means "return to no-dictionary mode".
 *  Note 1 : Currently, only one dictionary can be managed.
 *           Referencing a new dictionary effectively "discards" any previous one.
 *  Note 2 : CDict is just referenced, its lifetime must outlive its usage within CCtx. *)
function ZSTD_CCtx_refCDict(cctx: ZSTD_CCtx; cdict: ZSTD_CDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_refCDict;{$ENDIF}

(*! ZSTD_CCtx_refPrefix() :
 *  Reference a prefix (single-usage dictionary) for next compressed frame.
 *  A prefix is **only used once**. Tables are discarded at end of frame (ZSTD_e_end).
 *  Decompression will need same prefix to properly regenerate data.
 *  Compressing with a prefix is similar in outcome as performing a diff and compressing it,
 *  but performs much faster, especially during decompression (compression speed is tunable with compression level).
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Special: Adding any prefix (including NULL) invalidates any previous prefix or dictionary
 *  Note 1 : Prefix buffer is referenced. It **must** outlive compression.
 *           Its content must remain unmodified during compression.
 *  Note 2 : If the intention is to diff some large src data blob with some prior version of itself,
 *           ensure that the window size is large enough to contain the entire source.
 *           See ZSTD_c_windowLog.
 *  Note 3 : Referencing a prefix involves building tables, which are dependent on compression parameters.
 *           It's a CPU consuming operation, with non-negligible impact on latency.
 *           If there is a need to use the same prefix multiple times, consider loadDictionary instead.
 *  Note 4 : By default, the prefix is interpreted as raw content (ZSTD_dm_rawContent).
 *           Use experimental ZSTD_CCtx_refPrefix_advanced() to alter dictionary interpretation. *)
function ZSTD_CCtx_refPrefix(cctx: ZSTD_CCtx; prefix: Pointer; prefixSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_CCtx_refCDict;{$ENDIF}

(*! ZSTD_DCtx_loadDictionary() :
 *  Create an internal DDict from dict buffer,
 *  to be used to decompress next frames.
 *  The dictionary remains valid for all future frames, until explicitly invalidated.
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Special : Adding a NULL (or 0-size) dictionary invalidates any previous dictionary,
 *            meaning "return to no-dictionary mode".
 *  Note 1 : Loading a dictionary involves building tables,
 *           which has a non-negligible impact on CPU usage and latency.
 *           It's recommended to "load once, use many times", to amortize the cost
 *  Note 2 :`dict` content will be copied internally, so `dict` can be released after loading.
 *           Use ZSTD_DCtx_loadDictionary_byReference() to reference dictionary content instead.
 *  Note 3 : Use ZSTD_DCtx_loadDictionary_advanced() to take control of
 *           how dictionary content is loaded and interpreted.
 *)
function ZSTD_DCtx_loadDictionary(dctx: ZSTD_DCtx; dict: Pointer; dictSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DCtx_loadDictionary;{$ENDIF}

(*! ZSTD_DCtx_refDDict() :
 *  Reference a prepared dictionary, to be used to decompress next frames.
 *  The dictionary remains active for decompression of future frames using same DCtx.
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Note 1 : Currently, only one dictionary can be managed.
 *           Referencing a new dictionary effectively "discards" any previous one.
 *  Special: referencing a NULL DDict means "return to no-dictionary mode".
 *  Note 2 : DDict is just referenced, its lifetime must outlive its usage from DCtx.
 *)
function ZSTD_DCtx_refDDict(dctx: ZSTD_DCtx; ddict: ZSTD_DDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DCtx_refDDict;{$ENDIF}

(*! ZSTD_DCtx_refPrefix() :
 *  Reference a prefix (single-usage dictionary) to decompress next frame.
 *  This is the reverse operation of ZSTD_CCtx_refPrefix(),
 *  and must use the same prefix as the one used during compression.
 *  Prefix is **only used once**. Reference is discarded at end of frame.
 *  End of frame is reached when ZSTD_decompressStream() returns 0.
 * @result : 0, or an error code (which can be tested with ZSTD_isError()).
 *  Note 1 : Adding any prefix (including NULL) invalidates any previously set prefix or dictionary
 *  Note 2 : Prefix buffer is referenced. It **must** outlive decompression.
 *           Prefix buffer must remain unmodified up to the end of frame,
 *           reached when ZSTD_decompressStream() returns 0.
 *  Note 3 : By default, the prefix is treated as raw content (ZSTD_dm_rawContent).
 *           Use ZSTD_CCtx_refPrefix_advanced() to alter dictMode (Experimental section)
 *  Note 4 : Referencing a raw content prefix has almost no cpu nor memory cost.
 *           A full dictionary is more costly, as it requires building tables.
 *)
function ZSTD_DCtx_refPrefix(dctx: ZSTD_DCtx; prefix: Pointer; prefixSize: size_t): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_DCtx_refPrefix;{$ENDIF}

(* ===   Memory management   === *)

(*! ZSTD_sizeof_*() :
 *  These functions give the _current_ memory usage of selected object.
 *  Note that object memory usage can evolve (increase or decrease) over time. *)
function ZSTD_sizeof_CCtx(cctx: ZSTD_CCtx): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_sizeof_CCtx;{$ENDIF}
function ZSTD_sizeof_DCtx(dctx: ZSTD_DCtx): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_sizeof_DCtx;{$ENDIF}
function ZSTD_sizeof_CStream(zcs: ZSTD_CStream ): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_sizeof_CStream;{$ENDIF}
function ZSTD_sizeof_DStream(zds: ZSTD_DStream): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_sizeof_DStream;{$ENDIF}
function ZSTD_sizeof_CDict(cdict: ZSTD_CDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_sizeof_CDict;{$ENDIF}
function ZSTD_sizeof_DDict(ddict: ZSTD_DDict): size_t; {$IFDEF ZSTD_STATIC_LINKING}cdecl; external ZSTDDllName name sZSTD_sizeof_DDict;{$ENDIF}

function ZSTDIsLoaded : boolean;

implementation

const
  ZSTD_error_no_error                      = 0;
  ZSTD_error_GENERIC                       = 1;
  ZSTD_error_prefix_unknown                = 10;
  ZSTD_error_version_unsupported           = 12;
  ZSTD_error_frameParameter_unsupported    = 14;
  ZSTD_error_frameParameter_windowTooLarge = 16;
  ZSTD_error_corruption_detected           = 20;
  ZSTD_error_checksum_wrong                = 22;
  ZSTD_error_dictionary_corrupted          = 30;
  ZSTD_error_dictionary_wrong              = 32;
  ZSTD_error_dictionaryCreation_failed     = 34;
  ZSTD_error_parameter_unsupported         = 40;
  ZSTD_error_parameter_outOfBound          = 42;
  ZSTD_error_tableLog_tooLarge             = 44;
  ZSTD_error_maxSymbolValue_tooLarge       = 46;
  ZSTD_error_maxSymbolValue_tooSmall       = 48;
  ZSTD_error_stage_wrong                   = 60;
  ZSTD_error_init_missing                  = 62;
  ZSTD_error_memory_allocation             = 64;
  ZSTD_error_workSpace_tooSmall            = 66;
  ZSTD_error_dstSize_tooSmall              = 70;
  ZSTD_error_srcSize_wrong                 = 72;
  ZSTD_error_dstBuffer_null                = 74;

function GetExceptionMessage(const AFunctionName: string; ACode: NativeInt): string;
begin
  Result := AFunctionName + ' failed with error ' + IntToStr(ACode) + ': ' + string(ZSTD_getErrorName(ACode));
end;

constructor EZSTDException.Create(const AFunctionName: string; ACode: NativeInt);
begin
  FCode := ACode;
  inherited Create(GetExceptionMessage(AFunctionName, ACode));
end;

procedure ZSTDError(const AFunctionName: string; ACode: size_t);
begin
  case -ACode of
    ZSTD_error_frameParameter_windowTooLarge,
    ZSTD_error_memory_allocation:
      raise EOutOfMemory.Create(GetExceptionMessage(AFunctionName, ACode));
  else
    raise EZSTDException.Create(AFunctionName, ACode);
  end;
end;

function ZSTDCheck(const AFunctionName: string; ACode: size_t): size_t;
begin
  Result := ACode;
  if ZSTD_isError(ACode) <> 0 then
    ZSTDError(AFunctionName, ACode);
end;

{$IFNDEF ZSTD_STATIC_LINKING}
type
  TZSTD_versionNumber = function: unsigned; cdecl;
  TZSTD_versionString = function: PAnsiChar; cdecl;
  TZSTD_compress = function(dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; compressionLevel: int): size_t; cdecl;
  TZSTD_decompress = function(dst: Pointer; dstCapacity: size_t; src: Pointer; compressedSize: size_t): size_t; cdecl;
  TZSTD_getFrameContentSize = function(src: Pointer; srcSize: size_t): Int64; cdecl;
  TZSTD_getDecompressedSize = function(src: Pointer; srcSize: size_t): Int64; cdecl;
  TZSTD_findFrameCompressedSize = function(src: Pointer; srcSize: size_t): size_t; cdecl;
  TZSTD_compressBound = function(srcSize: size_t): size_t; cdecl;
  TZSTD_isError = function(code: size_t): unsigned; cdecl;
  TZSTD_getErrorName = function(code: size_t): PAnsiChar; cdecl;
  TZSTD_minCLevel = function: int; cdecl;
  TZSTD_maxCLevel = function: int; cdecl;
  TZSTD_createCCtx = function: ZSTD_CCtx; cdecl;
  TZSTD_freeCCtx = function(cctx: ZSTD_CCtx): size_t; cdecl;
  TZSTD_compressCCtx = function(ctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; compressionLevel: int): size_t; cdecl;
  TZSTD_createDCtx = function: ZSTD_DCtx; cdecl;
  TZSTD_freeDCtx = function(dctx: ZSTD_DCtx): size_t; cdecl;
  TZSTD_decompressDCtx = function(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t; cdecl;
  TZSTD_cParam_getBounds = function(cParam: ZSTD_cParameter): ZSTD_bounds; cdecl;
  TZSTD_CCtx_setParameter = function(cctx: ZSTD_CCtx; param: ZSTD_cParameter; value: int): size_t; cdecl;
  TZSTD_CCtx_setPledgedSrcSize = function(cctx: ZSTD_CCtx; pledgedSrcSize: Int64): size_t; cdecl;
  TZSTD_CCtx_reset = function(cctx: ZSTD_CCtx; reset: ZSTD_ResetDirective): size_t; cdecl;
  TZSTD_compress2 = function(cctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t; cdecl;
  TZSTD_dParam_getBounds = function(dParam: ZSTD_dParameter): ZSTD_bounds; cdecl;
  TZSTD_DCtx_setParameter = function(dctx: ZSTD_DCtx; param: ZSTD_dParameter; value: int): size_t; cdecl;
  TZSTD_DCtx_reset = function(dctx: ZSTD_DCtx; reset: ZSTD_ResetDirective): size_t; cdecl;
  TZSTD_createCStream = function: ZSTD_CStream; cdecl;
  TZSTD_freeCStream = function(zcs: ZSTD_CStream): size_t; cdecl;
  TZSTD_compressStream2 = function(cctx: ZSTD_CCtx; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer; endOp: ZSTD_EndDirective): size_t; cdecl;
  TZSTD_CStreamInSize = function: size_t; cdecl;
  TZSTD_CStreamOutSize = function: size_t; cdecl;
  TZSTD_initCStream = function(zcs: ZSTD_CStream; compressionLevel: int): size_t; cdecl;
  TZSTD_compressStream = function(zcs: ZSTD_CStream; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer): size_t; cdecl;
  TZSTD_flushStream = function(zcs: ZSTD_CStream; var output: ZSTD_outBuffer): size_t; cdecl;
  TZSTD_endStream = function(zcs: ZSTD_CStream; var output: ZSTD_outBuffer): size_t; cdecl;
  TZSTD_createDStream = function: ZSTD_DStream; cdecl;
  TZSTD_freeDStream = function(zds: ZSTD_DStream): size_t; cdecl;
  TZSTD_initDStream = function(zds: ZSTD_DStream): size_t; cdecl;
  TZSTD_decompressStream = function(zds: ZSTD_DStream; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer): size_t; cdecl;
  TZSTD_DStreamInSize = function: size_t; cdecl;
  TZSTD_DStreamOutSize = function: size_t; cdecl;
  TZSTD_compress_usingDict = function(ctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; dict: Pointer; dictSize: size_t; compressionLevel: int): size_t; cdecl;
  TZSTD_decompress_usingDict = function(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; dict: Pointer; dictSize: size_t): size_t; cdecl;
  TZSTD_createCDict = function(dictBuffer: Pointer; dictSize: size_t; compressionLevel: int): ZSTD_CDict; cdecl;
  TZSTD_freeCDict = function(CDict: ZSTD_CDict): size_t; cdecl;
  TZSTD_compress_usingCDict = function(cctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; cdict: ZSTD_CDict): size_t; cdecl;
  TZSTD_createDDict = function(dictBuffer: Pointer; dictSize: size_t): ZSTD_DDict; cdecl;
  TZSTD_freeDDict = function(ddict: ZSTD_DDict): size_t; cdecl;
  TZSTD_decompress_usingDDict = function(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; ddict: ZSTD_DDict): size_t; cdecl;
  TZSTD_getDictID_fromDict = function(dict: Pointer; dictSize: size_t): unsigned; cdecl;
  TZSTD_getDictID_fromDDict = function(ddict: ZSTD_DDict): unsigned; cdecl;
  TZSTD_getDictID_fromFrame = function(src: Pointer; srcSize: size_t): unsigned; cdecl;
  TZSTD_CCtx_loadDictionary = function(cctx: ZSTD_CCtx; dict: Pointer; dictSize: size_t): size_t; cdecl;
  TZSTD_CCtx_refCDict = function(cctx: ZSTD_CCtx; cdict: ZSTD_CDict): size_t; cdecl;
  TZSTD_CCtx_refPrefix = function(cctx: ZSTD_CCtx; prefix: Pointer; prefixSize: size_t): size_t; cdecl;
  TZSTD_DCtx_loadDictionary = function(dctx: ZSTD_DCtx; dict: Pointer; dictSize: size_t): size_t; cdecl;
  TZSTD_DCtx_refDDict = function(dctx: ZSTD_DCtx; ddict: ZSTD_DDict): size_t; cdecl;
  TZSTD_DCtx_refPrefix = function(dctx: ZSTD_DCtx; prefix: Pointer; prefixSize: size_t): size_t; cdecl;
  TZSTD_sizeof_CCtx = function(cctx: ZSTD_CCtx): size_t; cdecl;
  TZSTD_sizeof_DCtx = function(dctx: ZSTD_DCtx): size_t; cdecl;
  TZSTD_sizeof_CStream = function(zcs: ZSTD_CStream ): size_t; cdecl;
  TZSTD_sizeof_DStream = function(zds: ZSTD_DStream): size_t; cdecl;
  TZSTD_sizeof_CDict = function(cdict: ZSTD_CDict): size_t; cdecl;
  TZSTD_sizeof_DDict = function(ddict: ZSTD_DDict): size_t; cdecl;

var
  ZSTDLock: TRTLCriticalSection;
  ZSTD: HMODULE;

  _ZSTD_versionNumber: TZSTD_versionNumber;
  _ZSTD_versionString: TZSTD_versionString;
  _ZSTD_compress: TZSTD_compress;
  _ZSTD_decompress: TZSTD_decompress;
  _ZSTD_getFrameContentSize: TZSTD_getFrameContentSize;
  _ZSTD_getDecompressedSize: TZSTD_getDecompressedSize;
  _ZSTD_findFrameCompressedSize: TZSTD_findFrameCompressedSize;
  _ZSTD_compressBound: TZSTD_compressBound;
  _ZSTD_isError: TZSTD_isError;
  _ZSTD_getErrorName: TZSTD_getErrorName;
  _ZSTD_minCLevel: TZSTD_minCLevel;
  _ZSTD_maxCLevel: TZSTD_maxCLevel;
  _ZSTD_createCCtx: TZSTD_createCCtx;
  _ZSTD_freeCCtx: TZSTD_freeCCtx;
  _ZSTD_compressCCtx: TZSTD_compressCCtx;
  _ZSTD_createDCtx: TZSTD_createDCtx;
  _ZSTD_freeDCtx: TZSTD_freeDCtx;
  _ZSTD_decompressDCtx: TZSTD_decompressDCtx;
  _ZSTD_cParam_getBounds: TZSTD_cParam_getBounds;
  _ZSTD_CCtx_setParameter: TZSTD_CCtx_setParameter;
  _ZSTD_CCtx_setPledgedSrcSize: TZSTD_CCtx_setPledgedSrcSize;
  _ZSTD_CCtx_reset: TZSTD_CCtx_reset;
  _ZSTD_compress2: TZSTD_compress2;
  _ZSTD_dParam_getBounds: TZSTD_dParam_getBounds;
  _ZSTD_DCtx_setParameter: TZSTD_DCtx_setParameter;
  _ZSTD_DCtx_reset: TZSTD_DCtx_reset;
  _ZSTD_createCStream: TZSTD_createCStream;
  _ZSTD_freeCStream: TZSTD_freeCStream;
  _ZSTD_compressStream2: TZSTD_compressStream2;
  _ZSTD_CStreamInSize: TZSTD_CStreamInSize;
  _ZSTD_CStreamOutSize: TZSTD_CStreamOutSize;
  _ZSTD_initCStream: TZSTD_initCStream;
  _ZSTD_compressStream: TZSTD_compressStream;
  _ZSTD_flushStream: TZSTD_flushStream;
  _ZSTD_endStream: TZSTD_endStream;
  _ZSTD_createDStream: TZSTD_createDStream;
  _ZSTD_freeDStream: TZSTD_freeDStream;
  _ZSTD_initDStream: TZSTD_initDStream;
  _ZSTD_decompressStream: TZSTD_decompressStream;
  _ZSTD_DStreamInSize: TZSTD_DStreamInSize;
  _ZSTD_DStreamOutSize: TZSTD_DStreamOutSize;
  _ZSTD_compress_usingDict: TZSTD_compress_usingDict;
  _ZSTD_decompress_usingDict: TZSTD_decompress_usingDict;
  _ZSTD_createCDict: TZSTD_createCDict;
  _ZSTD_freeCDict: TZSTD_freeCDict;
  _ZSTD_compress_usingCDict: TZSTD_compress_usingCDict;
  _ZSTD_createDDict: TZSTD_createDDict;
  _ZSTD_freeDDict: TZSTD_freeDDict;
  _ZSTD_decompress_usingDDict: TZSTD_decompress_usingDDict;
  _ZSTD_getDictID_fromDict: TZSTD_getDictID_fromDict;
  _ZSTD_getDictID_fromDDict: TZSTD_getDictID_fromDDict;
  _ZSTD_getDictID_fromFrame: TZSTD_getDictID_fromFrame;
  _ZSTD_CCtx_loadDictionary: TZSTD_CCtx_loadDictionary;
  _ZSTD_CCtx_refCDict: TZSTD_CCtx_refCDict;
  _ZSTD_CCtx_refPrefix: TZSTD_CCtx_refPrefix;
  _ZSTD_DCtx_loadDictionary: TZSTD_DCtx_loadDictionary;
  _ZSTD_DCtx_refDDict: TZSTD_DCtx_refDDict;
  _ZSTD_DCtx_refPrefix: TZSTD_DCtx_refPrefix;
  _ZSTD_sizeof_CCtx: TZSTD_sizeof_CCtx;
  _ZSTD_sizeof_DCtx: TZSTD_sizeof_DCtx;
  _ZSTD_sizeof_CStream: TZSTD_sizeof_CStream;
  _ZSTD_sizeof_DStream: TZSTD_sizeof_DStream;
  _ZSTD_sizeof_CDict: TZSTD_sizeof_CDict;
  _ZSTD_sizeof_DDict: TZSTD_sizeof_DDict;

procedure InitZSTD;
begin
  EnterCriticalSection(ZSTDLock);
  try
    if ZSTD <> 0 then
      Exit;
    ZSTD := SafeLoadLibrary(ZSTDDllName);
    if ZSTD = 0 then
      Exit;

    _ZSTD_versionNumber                       := GetProcAddress(ZSTD, sZSTD_versionNumber);
    _ZSTD_versionString                       := GetProcAddress(ZSTD, sZSTD_versionString);
    _ZSTD_compress                            := GetProcAddress(ZSTD, sZSTD_compress);
    _ZSTD_decompress                          := GetProcAddress(ZSTD, sZSTD_decompress);
    _ZSTD_getFrameContentSize                 := GetProcAddress(ZSTD, sZSTD_getFrameContentSize);
    _ZSTD_getDecompressedSize                 := GetProcAddress(ZSTD, sZSTD_getDecompressedSize);
    _ZSTD_findFrameCompressedSize             := GetProcAddress(ZSTD, sZSTD_findFrameCompressedSize);
    _ZSTD_compressBound                       := GetProcAddress(ZSTD, sZSTD_compressBound);
    _ZSTD_isError                             := GetProcAddress(ZSTD, sZSTD_isError);
    _ZSTD_getErrorName                        := GetProcAddress(ZSTD, sZSTD_getErrorName);
    _ZSTD_minCLevel                           := GetProcAddress(ZSTD, sZSTD_minCLevel);
    _ZSTD_maxCLevel                           := GetProcAddress(ZSTD, sZSTD_maxCLevel);
    _ZSTD_createCCtx                          := GetProcAddress(ZSTD, sZSTD_createCCtx);
    _ZSTD_freeCCtx                            := GetProcAddress(ZSTD, sZSTD_freeCCtx);
    _ZSTD_compressCCtx                        := GetProcAddress(ZSTD, sZSTD_compressCCtx);
    _ZSTD_createDCtx                          := GetProcAddress(ZSTD, sZSTD_createDCtx);
    _ZSTD_freeDCtx                            := GetProcAddress(ZSTD, sZSTD_freeDCtx);
    _ZSTD_decompressDCtx                      := GetProcAddress(ZSTD, sZSTD_decompressDCtx);
    _ZSTD_cParam_getBounds                    := GetProcAddress(ZSTD, sZSTD_cParam_getBounds);
    _ZSTD_CCtx_setParameter                   := GetProcAddress(ZSTD, sZSTD_CCtx_setParameter);
    _ZSTD_CCtx_setPledgedSrcSize              := GetProcAddress(ZSTD, sZSTD_CCtx_setPledgedSrcSize);
    _ZSTD_CCtx_reset                          := GetProcAddress(ZSTD, sZSTD_CCtx_reset);
    _ZSTD_compress2                           := GetProcAddress(ZSTD, sZSTD_compress2);
    _ZSTD_dParam_getBounds                    := GetProcAddress(ZSTD, sZSTD_dParam_getBounds);
    _ZSTD_DCtx_setParameter                   := GetProcAddress(ZSTD, sZSTD_DCtx_setParameter);
    _ZSTD_DCtx_reset                          := GetProcAddress(ZSTD, sZSTD_DCtx_reset);
    _ZSTD_createCStream                       := GetProcAddress(ZSTD, sZSTD_createCStream);
    _ZSTD_freeCStream                         := GetProcAddress(ZSTD, sZSTD_freeCStream);
    _ZSTD_compressStream2                     := GetProcAddress(ZSTD, sZSTD_compressStream2);
    _ZSTD_CStreamInSize                       := GetProcAddress(ZSTD, sZSTD_CStreamInSize);
    _ZSTD_CStreamOutSize                      := GetProcAddress(ZSTD, sZSTD_CStreamOutSize);
    _ZSTD_initCStream                         := GetProcAddress(ZSTD, sZSTD_initCStream);
    _ZSTD_compressStream                      := GetProcAddress(ZSTD, sZSTD_compressStream);
    _ZSTD_flushStream                         := GetProcAddress(ZSTD, sZSTD_flushStream);
    _ZSTD_endStream                           := GetProcAddress(ZSTD, sZSTD_endStream);
    _ZSTD_createDStream                       := GetProcAddress(ZSTD, sZSTD_createDStream);
    _ZSTD_freeDStream                         := GetProcAddress(ZSTD, sZSTD_freeDStream);
    _ZSTD_initDStream                         := GetProcAddress(ZSTD, sZSTD_initDStream);
    _ZSTD_decompressStream                    := GetProcAddress(ZSTD, sZSTD_decompressStream);
    _ZSTD_DStreamInSize                       := GetProcAddress(ZSTD, sZSTD_DStreamInSize);
    _ZSTD_DStreamOutSize                      := GetProcAddress(ZSTD, sZSTD_DStreamOutSize);
    _ZSTD_compress_usingDict                  := GetProcAddress(ZSTD, sZSTD_compress_usingDict);
    _ZSTD_decompress_usingDict                := GetProcAddress(ZSTD, sZSTD_decompress_usingDict);
    _ZSTD_createCDict                         := GetProcAddress(ZSTD, sZSTD_createCDict);
    _ZSTD_freeCDict                           := GetProcAddress(ZSTD, sZSTD_freeCDict);
    _ZSTD_compress_usingCDict                 := GetProcAddress(ZSTD, sZSTD_compress_usingCDict);
    _ZSTD_createDDict                         := GetProcAddress(ZSTD, sZSTD_createDDict);
    _ZSTD_freeDDict                           := GetProcAddress(ZSTD, sZSTD_freeDDict);
    _ZSTD_decompress_usingDDict               := GetProcAddress(ZSTD, sZSTD_decompress_usingDDict);
    _ZSTD_getDictID_fromDict                  := GetProcAddress(ZSTD, sZSTD_getDictID_fromDict);
    _ZSTD_getDictID_fromDDict                 := GetProcAddress(ZSTD, sZSTD_getDictID_fromDDict);
    _ZSTD_getDictID_fromFrame                 := GetProcAddress(ZSTD, sZSTD_getDictID_fromFrame);
    _ZSTD_CCtx_loadDictionary                 := GetProcAddress(ZSTD, sZSTD_CCtx_loadDictionary);
    _ZSTD_CCtx_refCDict                       := GetProcAddress(ZSTD, sZSTD_CCtx_refCDict);
    _ZSTD_CCtx_refPrefix                      := GetProcAddress(ZSTD, sZSTD_CCtx_refPrefix);
    _ZSTD_DCtx_loadDictionary                 := GetProcAddress(ZSTD, sZSTD_DCtx_loadDictionary);
    _ZSTD_DCtx_refDDict                       := GetProcAddress(ZSTD, sZSTD_DCtx_refDDict);
    _ZSTD_DCtx_refPrefix                      := GetProcAddress(ZSTD, sZSTD_DCtx_refPrefix);
    _ZSTD_sizeof_CCtx                         := GetProcAddress(ZSTD, sZSTD_sizeof_CCtx);
    _ZSTD_sizeof_DCtx                         := GetProcAddress(ZSTD, sZSTD_sizeof_DCtx);
    _ZSTD_sizeof_CStream                      := GetProcAddress(ZSTD, sZSTD_sizeof_CStream);
    _ZSTD_sizeof_DStream                      := GetProcAddress(ZSTD, sZSTD_sizeof_DStream);
    _ZSTD_sizeof_CDict                        := GetProcAddress(ZSTD, sZSTD_sizeof_CDict);
    _ZSTD_sizeof_DDict                        := GetProcAddress(ZSTD, sZSTD_sizeof_DDict);
  finally
    LeaveCriticalSection(ZSTDLock);
  end;
end;

procedure DoneZSTD;
begin
  if ZSTD <> 0 then FreeLibrary(ZSTD);
end;

function ZSTD_versionNumber: unsigned;
begin
  InitZSTD;
  if Assigned(@_ZSTD_versionNumber) then
    Result := _ZSTD_versionNumber
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_versionString: PAnsiChar;
begin
  InitZSTD;
  if Assigned(@_ZSTD_versionString) then
    Result := _ZSTD_versionString
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compress(dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; compressionLevel: int): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compress) then
    Result := _ZSTD_compress(dst, dstCapacity, src, srcSize, compressionLevel)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_decompress(dst: Pointer; dstCapacity: size_t; src: Pointer; compressedSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_decompress) then
    Result := _ZSTD_decompress(dst, dstCapacity, src, compressedSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_getFrameContentSize(src: Pointer; srcSize: size_t): Int64;
begin
  InitZSTD;
  if Assigned(@_ZSTD_getFrameContentSize) then
    Result := _ZSTD_getFrameContentSize(src, srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_getDecompressedSize(src: Pointer; srcSize: size_t): Int64;
begin
  InitZSTD;
  if Assigned(@_ZSTD_getDecompressedSize) then
    Result := _ZSTD_getDecompressedSize(src, srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_findFrameCompressedSize(src: Pointer; srcSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_findFrameCompressedSize) then
    Result := _ZSTD_findFrameCompressedSize(src, srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compressBound(srcSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compressBound) then
    Result := _ZSTD_compressBound(srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_isError(code: size_t): unsigned;
begin
  InitZSTD;
  if Assigned(@_ZSTD_isError) then
    Result := _ZSTD_isError(code)
  else
    Result := 1;
end;

function ZSTD_getErrorName(code: size_t): PAnsiChar;
begin
  InitZSTD;
  if Assigned(@_ZSTD_getErrorName) then
    Result := _ZSTD_getErrorName(code)
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_minCLevel: int;
begin
  InitZSTD;
  if Assigned(@_ZSTD_minCLevel) then
    Result := _ZSTD_minCLevel
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_maxCLevel: int;
begin
  InitZSTD;
  if Assigned(@_ZSTD_maxCLevel) then
    Result := _ZSTD_maxCLevel
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_createCCtx: ZSTD_CCtx;
begin
  InitZSTD;
  if Assigned(@_ZSTD_createCCtx) then
    Result := _ZSTD_createCCtx
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_freeCCtx(cctx: ZSTD_CCtx): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_freeCCtx) then
    Result := _ZSTD_freeCCtx(cctx)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compressCCtx(ctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; compressionLevel: int): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compressCCtx) then
    Result := _ZSTD_compressCCtx(ctx, dst, dstCapacity, src, srcSize, compressionLevel)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_createDCtx: ZSTD_DCtx;
begin
  InitZSTD;
  if Assigned(@_ZSTD_createDCtx) then
    Result := _ZSTD_createDCtx
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_freeDCtx(dctx: ZSTD_DCtx): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_freeDCtx) then
    Result := _ZSTD_freeDCtx(dctx)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_decompressDCtx(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_decompressDCtx) then
    Result := _ZSTD_decompressDCtx(dctx, dst, dstCapacity, src, srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_cParam_getBounds(cParam: ZSTD_cParameter): ZSTD_bounds;
begin
  InitZSTD;
  if Assigned(@_ZSTD_cParam_getBounds) then
    Result := _ZSTD_cParam_getBounds(cParam)
  else
    begin Result.error := 0; Result.lowerBound := 0; Result.upperBound := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CCtx_setParameter(cctx: ZSTD_CCtx; param: ZSTD_cParameter; value: int): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CCtx_setParameter) then
    Result := _ZSTD_CCtx_setParameter(cctx, param, value)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CCtx_setPledgedSrcSize(cctx: ZSTD_CCtx; pledgedSrcSize: Int64): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CCtx_setPledgedSrcSize) then
    Result := _ZSTD_CCtx_setPledgedSrcSize(cctx, pledgedSrcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CCtx_reset(cctx: ZSTD_CCtx; reset: ZSTD_ResetDirective): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CCtx_reset) then
    Result := _ZSTD_CCtx_reset(cctx, reset)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compress2(cctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compress2) then
    Result := _ZSTD_compress2(cctx, dst, dstCapacity, src, srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_dParam_getBounds(dParam: ZSTD_dParameter): ZSTD_bounds;
begin
  InitZSTD;
  if Assigned(@_ZSTD_dParam_getBounds) then
    Result := _ZSTD_dParam_getBounds(dParam)
  else
    begin Result.error := 0; Result.lowerBound := 0; Result.upperBound := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DCtx_setParameter(dctx: ZSTD_DCtx; param: ZSTD_dParameter; value: int): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DCtx_setParameter) then
    Result := _ZSTD_DCtx_setParameter(dctx, param, value)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DCtx_reset(dctx: ZSTD_DCtx; reset: ZSTD_ResetDirective): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DCtx_reset) then
    Result := _ZSTD_DCtx_reset(dctx, reset)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_createCStream: ZSTD_CStream;
begin
  InitZSTD;
  if Assigned(@_ZSTD_createCStream) then
    Result := _ZSTD_createCStream
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_freeCStream(zcs: ZSTD_CStream): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_freeCStream) then
    Result := _ZSTD_freeCStream(zcs)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compressStream2(cctx: ZSTD_CCtx; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer; endOp: ZSTD_EndDirective): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compressStream2) then
    Result := _ZSTD_compressStream2(cctx, output, input, endOp)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CStreamInSize: size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CStreamInSize) then
    Result := _ZSTD_CStreamInSize
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CStreamOutSize: size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CStreamOutSize) then
    Result := _ZSTD_CStreamOutSize
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_initCStream(zcs: ZSTD_CStream; compressionLevel: int): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_initCStream) then
    Result := _ZSTD_initCStream(zcs, compressionLevel)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compressStream(zcs: ZSTD_CStream; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compressStream) then
    Result := _ZSTD_compressStream(zcs, output, input)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_flushStream(zcs: ZSTD_CStream; var output: ZSTD_outBuffer): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_flushStream) then
    Result := _ZSTD_flushStream(zcs, output)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_endStream(zcs: ZSTD_CStream; var output: ZSTD_outBuffer): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_endStream) then
    Result := _ZSTD_endStream(zcs, output)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_createDStream: ZSTD_DStream;
begin
  InitZSTD;
  if Assigned(@_ZSTD_createDStream) then
    Result := _ZSTD_createDStream
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_freeDStream(zds: ZSTD_DStream): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_freeDStream) then
    Result := _ZSTD_freeDStream(zds)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_initDStream(zds: ZSTD_DStream): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_initDStream) then
    Result := _ZSTD_initDStream(zds)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_decompressStream(zds: ZSTD_DStream; var output: ZSTD_outBuffer; var input: ZSTD_inBuffer): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_decompressStream) then
    Result := _ZSTD_decompressStream(zds, output, input)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DStreamInSize: size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DStreamInSize) then
    Result := _ZSTD_DStreamInSize
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DStreamOutSize: size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DStreamOutSize) then
    Result := _ZSTD_DStreamOutSize
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compress_usingDict(ctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; dict: Pointer; dictSize: size_t; compressionLevel: int): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compress_usingDict) then
    Result := _ZSTD_compress_usingDict(ctx, dst, dstCapacity, src, srcSize, dict, dictSize, compressionLevel)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_decompress_usingDict(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; dict: Pointer; dictSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_decompress_usingDict) then
    Result := _ZSTD_decompress_usingDict(dctx, dst, dstCapacity, src, srcSize, dict, dictSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_createCDict(dictBuffer: Pointer; dictSize: size_t; compressionLevel: int): ZSTD_CDict;
begin
  InitZSTD;
  if Assigned(@_ZSTD_createCDict) then
    Result := _ZSTD_createCDict(dictBuffer, dictSize, compressionLevel)
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_freeCDict(CDict: ZSTD_CDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_freeCDict) then
    Result := _ZSTD_freeCDict(CDict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_compress_usingCDict(cctx: ZSTD_CCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; cdict: ZSTD_CDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_compress_usingCDict) then
    Result := _ZSTD_compress_usingCDict(cctx, dst, dstCapacity, src, srcSize, cdict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_createDDict(dictBuffer: Pointer; dictSize: size_t): ZSTD_DDict;
begin
  InitZSTD;
  if Assigned(@_ZSTD_createDDict) then
    Result := _ZSTD_createDDict(dictBuffer, dictSize)
  else
    begin Result := nil; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_freeDDict(ddict: ZSTD_DDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_freeDDict) then
    Result := _ZSTD_freeDDict(ddict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_decompress_usingDDict(dctx: ZSTD_DCtx; dst: Pointer; dstCapacity: size_t; src: Pointer; srcSize: size_t; ddict: ZSTD_DDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_decompress_usingDDict) then
    Result := _ZSTD_decompress_usingDDict(dctx, dst, dstCapacity, src, srcSize, ddict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_getDictID_fromDict(dict: Pointer; dictSize: size_t): unsigned;
begin
  InitZSTD;
  if Assigned(@_ZSTD_getDictID_fromDict) then
    Result := _ZSTD_getDictID_fromDict(dict, dictSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_getDictID_fromDDict(ddict: ZSTD_DDict): unsigned;
begin
  InitZSTD;
  if Assigned(@_ZSTD_getDictID_fromDDict) then
    Result := _ZSTD_getDictID_fromDDict(ddict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_getDictID_fromFrame(src: Pointer; srcSize: size_t): unsigned;
begin
  InitZSTD;
  if Assigned(@_ZSTD_getDictID_fromFrame) then
    Result := _ZSTD_getDictID_fromFrame(src, srcSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CCtx_loadDictionary(cctx: ZSTD_CCtx; dict: Pointer; dictSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CCtx_loadDictionary) then
    Result := _ZSTD_CCtx_loadDictionary(cctx, dict, dictSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CCtx_refCDict(cctx: ZSTD_CCtx; cdict: ZSTD_CDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CCtx_refCDict) then
    Result := _ZSTD_CCtx_refCDict(cctx, cdict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_CCtx_refPrefix(cctx: ZSTD_CCtx; prefix: Pointer; prefixSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_CCtx_refPrefix) then
    Result := _ZSTD_CCtx_refPrefix(cctx, prefix, prefixSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DCtx_loadDictionary(dctx: ZSTD_DCtx; dict: Pointer; dictSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DCtx_loadDictionary) then
    Result := _ZSTD_DCtx_loadDictionary(dctx, dict, dictSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DCtx_refDDict(dctx: ZSTD_DCtx; ddict: ZSTD_DDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DCtx_refDDict) then
    Result := _ZSTD_DCtx_refDDict(dctx, ddict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_DCtx_refPrefix(dctx: ZSTD_DCtx; prefix: Pointer; prefixSize: size_t): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_DCtx_refPrefix) then
    Result := _ZSTD_DCtx_refPrefix(dctx, prefix, prefixSize)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_sizeof_CCtx(cctx: ZSTD_CCtx): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_sizeof_CCtx) then
    Result := _ZSTD_sizeof_CCtx(cctx)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_sizeof_DCtx(dctx: ZSTD_DCtx): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_sizeof_DCtx) then
    Result := _ZSTD_sizeof_DCtx(dctx)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_sizeof_CStream(zcs: ZSTD_CStream ): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_sizeof_CStream) then
    Result := _ZSTD_sizeof_CStream(zcs)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_sizeof_DStream(zds: ZSTD_DStream): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_sizeof_DStream) then
    Result := _ZSTD_sizeof_DStream(zds)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_sizeof_CDict(cdict: ZSTD_CDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_sizeof_CDict) then
    Result := _ZSTD_sizeof_CDict(cdict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTD_sizeof_DDict(ddict: ZSTD_DDict): size_t;
begin
  InitZSTD;
  if Assigned(@_ZSTD_sizeof_DDict) then
    Result := _ZSTD_sizeof_DDict(ddict)
  else
    begin Result := 0; RaiseLastOSError(ERROR_PROC_NOT_FOUND); end;
end;

function ZSTDIsLoaded: boolean;
begin
  InitZSTD;
  Result := ZSTD <> 0;
end;

initialization
  ZSTD := 0;
  InitializeCriticalSection(ZSTDLock);

finalization
  DoneZSTD;
  DeleteCriticalSection(ZSTDLock);
{$ENDIF}

end.
