@ECHO OFF

ECHO ---------- Compile BCC ----------------------------------------------------

SET LIBOBJECTS=^
  src/common/constants.c^
  src/common/context.c^
  src/common/dictionary.c^
  src/common/platform.c^
  src/common/transform.c^
  src/dec/bit_reader.c^
  src/dec/decode.c^
  src/dec/huffman.c^
  src/dec/state.c^
  src/enc/backward_references.c^
  src/enc/backward_references_hq.c^
  src/enc/bit_cost.c^
  src/enc/block_splitter.c^
  src/enc/brotli_bit_stream.c^
  src/enc/cluster.c^
  src/enc/command.c^
  src/enc/compress_fragment.c^
  src/enc/compress_fragment_two_pass.c^
  src/enc/dictionary_hash.c^
  src/enc/encode.c^
  src/enc/encoder_dict.c^
  src/enc/entropy_encode.c^
  src/enc/fast_log.c^
  src/enc/histogram.c^
  src/enc/literal_cost.c^
  src/enc/memory.c^
  src/enc/metablock.c^
  src/enc/static_dict.c^
  src/enc/utf8_util.c

SET INCLUDE_DIR=src/include

ECHO ---------- Compile BCC 32 -------------------------------------------------

SET BCC32=bcc32.exe
SET BCC32_OUTDIR=static\bcc32

REM  -w      Display all warnings
REM  -6      Generate Pentium Pro instructions
REM  -O2     Generate fastest possible code
REM  -d      Merge duplicate strings
REM  -mm     Ignore system header files while generating dependency info
REM  -c      Compile to object file only, do not link
REM  -I      Set the include file search path 
REM  -n      Set output directory for object files

SET BCC32_FLAGS=-w -6 -O2 -d -mm -c -I%INCLUDE_DIR%  -n%BCC32_OUTDIR% -DBROTLI_BUILD_32_BIT

"%BCC32%" %BCC32_FLAGS% %LIBOBJECTS%
REM Remove dependency info files *.d
DEL /F /Q %BCC32_OUTDIR%\*.d

ECHO ---------- Compile BCC 64 ----------------------------------------

SET BCC64=bcc64.exe
SET BCC64_OUTDIR=static\bcc64

REM  -mm     Ignore system header files while generating dependency info
REM          (add this option is you are using old non-LLVM compiler)

REM  -w      Display all warnings
REM  -O2     Generate fastest possible code
REM  -d      Merge duplicate strings
REM  -c      Compile to object file only, do not link
REM  -I      Set the include file search path 
REM  -output-dir <directory> Write object files to <directory>

SET BCC64_FLAGS=-w -O2 -d -c -u- -I%INCLUDE_DIR% -output-dir %BCC64_OUTDIR% -DBROTLI_BUILD_64_BIT

"%BCC64%" %BCC64_FLAGS% %LIBOBJECTS%
