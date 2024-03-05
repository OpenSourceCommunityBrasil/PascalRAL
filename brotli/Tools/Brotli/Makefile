CC = gcc

LIB_SOURCES = \
  src/common/constants.c \
  src/common/context.c \
  src/common/dictionary.c \
  src/common/platform.c \
  src/common/transform.c \
  src/dec/bit_reader.c \
  src/dec/decode.c \
  src/dec/huffman.c \
  src/dec/state.c \
  src/enc/backward_references.c \
  src/enc/backward_references_hq.c \
  src/enc/bit_cost.c \
  src/enc/block_splitter.c \
  src/enc/brotli_bit_stream.c \
  src/enc/cluster.c \
  src/enc/command.c \
  src/enc/compress_fragment.c \
  src/enc/compress_fragment_two_pass.c \
  src/enc/dictionary_hash.c \
  src/enc/encode.c \
  src/enc/encoder_dict.c \
  src/enc/entropy_encode.c \
  src/enc/fast_log.c \
  src/enc/histogram.c \
  src/enc/literal_cost.c \
  src/enc/memory.c \
  src/enc/metablock.c \
  src/enc/static_dict.c \
  src/enc/utf8_util.c

INCLUDE = src/include

CFLAGS = -O2 -lm -fPIC -c

ifeq ($(shell uname -m), x86_64)
  OUTPUTDIR = static/gcc64
else
  OUTPUTDIR = static/gcc32
endif

.PHONY: lib

lib:
	@cd $(OUTPUTDIR); $(CC) $(CFLAGS) -I$(patsubst %,../../%,$(INCLUDE)) $(patsubst %,../../%,$(LIB_SOURCES))
