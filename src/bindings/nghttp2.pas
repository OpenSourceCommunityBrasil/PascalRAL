{ Object Pascal binding for nghttp2, the HTTP/2 framing library (nghttp2.org).

  Translated against lib/includes/nghttp2/nghttp2.h of nghttp2 1.59 (the oldest
  release accepted: NGHTTP2Load refuses an older library) and checked against
  1.68: the API is stable from 1.0 on and the records only grow at their end.
  Only the parts a server and a client over memory buffers need are here:
  sessions fed with nghttp2_session_mem_recv/mem_send, the callbacks, HEADERS,
  DATA with a data provider (deferred and NO_COPY), trailers, SETTINGS, RST,
  GOAWAY, PING, the HPACK coder (for the RFC 7541 vectors) and the options
  against the known floods.

  1. Calling convention is cdecl on every platform.
  2. Records follow the C layout with natural alignment ($PACKRECORDS C /
     $ALIGN 8); size_t is NativeUInt and ssize_t NativeInt.
  3. The library is loaded at run time by NGHTTP2Load; a symbol that only
     newer releases export (the flood options) is left nil when missing, and
     its caller tests it. }
unit nghttp2;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}
  {$PACKRECORDS C}
{$ENDIF}
{$IFNDEF FPC}
  {$ALIGN 8}
{$ENDIF}

interface

uses
  SysUtils
  {$IFDEF FPC}
  , DynLibs
  {$ELSE}
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, Posix.Dlfcn{$ENDIF}
  {$ENDIF};

const
  /// The oldest version accepted, as nghttp2_info.version_num (1.59.0).
  NGHTTP2_MIN_VERSION = $013B00;
  /// The first version without the CONTINUATION and rapid-reset DoS (1.68.1).
  NGHTTP2_SAFE_VERSION = $014401;

  /// The ALPN id of HTTP/2 over TLS.
  NGHTTP2_PROTO_VERSION_ID = 'h2';
  /// The client connection preface.
  NGHTTP2_CLIENT_MAGIC = 'PRI * HTTP/2.0'#13#10#13#10'SM'#13#10#13#10;
  /// The default initial window size.
  NGHTTP2_INITIAL_WINDOW_SIZE = 65535;

  // error codes of the library (negative)

  /// Invalid argument.
  NGHTTP2_ERR_INVALID_ARGUMENT = -501;
  /// The operation would block: the callback has no data now.
  NGHTTP2_ERR_WOULDBLOCK = -504;
  /// The data provider defers: call nghttp2_session_resume_data later.
  NGHTTP2_ERR_DEFERRED = -508;
  /// The stream was closed.
  NGHTTP2_ERR_STREAM_CLOSED = -510;
  /// The callback ended in a failure that ends the session.
  NGHTTP2_ERR_CALLBACK_FAILURE = -902;
  /// Resets the stream being read (from a header callback).
  NGHTTP2_ERR_TEMPORAL_CALLBACK_FAILURE = -521;
  /// The peer sent a bad client magic.
  NGHTTP2_ERR_BAD_CLIENT_MAGIC = -903;
  /// The peer flooded the session.
  NGHTTP2_ERR_FLOODED = -904;

  // frame types

  /// DATA.
  NGHTTP2_DATA = 0;
  /// HEADERS.
  NGHTTP2_HEADERS = 1;
  /// PRIORITY.
  NGHTTP2_PRIORITY = 2;
  /// RST_STREAM.
  NGHTTP2_RST_STREAM = 3;
  /// SETTINGS.
  NGHTTP2_SETTINGS = 4;
  /// PUSH_PROMISE.
  NGHTTP2_PUSH_PROMISE = 5;
  /// PING.
  NGHTTP2_PING = 6;
  /// GOAWAY.
  NGHTTP2_GOAWAY = 7;
  /// WINDOW_UPDATE.
  NGHTTP2_WINDOW_UPDATE = 8;
  /// CONTINUATION.
  NGHTTP2_CONTINUATION = 9;

  // frame flags

  /// No flag.
  NGHTTP2_FLAG_NONE = 0;
  /// END_STREAM.
  NGHTTP2_FLAG_END_STREAM = $01;
  /// END_HEADERS.
  NGHTTP2_FLAG_END_HEADERS = $04;
  /// ACK (SETTINGS, PING).
  NGHTTP2_FLAG_ACK = $01;

  // headers categories

  /// A request's first HEADERS.
  NGHTTP2_HCAT_REQUEST = 0;
  /// A response's first HEADERS.
  NGHTTP2_HCAT_RESPONSE = 1;
  /// A PUSH_PROMISE's response HEADERS.
  NGHTTP2_HCAT_PUSH_RESPONSE = 2;
  /// Any other HEADERS: trailers or a non-final response.
  NGHTTP2_HCAT_HEADERS = 3;

  // name/value flags

  /// No flag.
  NGHTTP2_NV_FLAG_NONE = 0;
  /// Never index the field (secrets).
  NGHTTP2_NV_FLAG_NO_INDEX = $01;
  /// The library may keep a pointer to the name instead of copying it.
  NGHTTP2_NV_FLAG_NO_COPY_NAME = $02;
  /// The library may keep a pointer to the value instead of copying it.
  NGHTTP2_NV_FLAG_NO_COPY_VALUE = $04;

  // data flags

  /// No flag.
  NGHTTP2_DATA_FLAG_NONE = 0;
  /// The provider has no more data.
  NGHTTP2_DATA_FLAG_EOF = $01;
  /// At EOF, do not set END_STREAM (trailers follow).
  NGHTTP2_DATA_FLAG_NO_END_STREAM = $02;
  /// The data goes through the send_data callback instead of being copied.
  NGHTTP2_DATA_FLAG_NO_COPY = $04;

  // settings ids

  /// SETTINGS_HEADER_TABLE_SIZE.
  NGHTTP2_SETTINGS_HEADER_TABLE_SIZE = $01;
  /// SETTINGS_ENABLE_PUSH.
  NGHTTP2_SETTINGS_ENABLE_PUSH = $02;
  /// SETTINGS_MAX_CONCURRENT_STREAMS.
  NGHTTP2_SETTINGS_MAX_CONCURRENT_STREAMS = $03;
  /// SETTINGS_INITIAL_WINDOW_SIZE.
  NGHTTP2_SETTINGS_INITIAL_WINDOW_SIZE = $04;
  /// SETTINGS_MAX_FRAME_SIZE.
  NGHTTP2_SETTINGS_MAX_FRAME_SIZE = $05;
  /// SETTINGS_MAX_HEADER_LIST_SIZE.
  NGHTTP2_SETTINGS_MAX_HEADER_LIST_SIZE = $06;

  // error codes of the protocol

  /// NO_ERROR.
  NGHTTP2_NO_ERROR = 0;
  /// PROTOCOL_ERROR.
  NGHTTP2_PROTOCOL_ERROR = 1;
  /// INTERNAL_ERROR.
  NGHTTP2_INTERNAL_ERROR = 2;
  /// FLOW_CONTROL_ERROR.
  NGHTTP2_FLOW_CONTROL_ERROR = 3;
  /// REFUSED_STREAM.
  NGHTTP2_REFUSED_STREAM = 7;
  /// CANCEL.
  NGHTTP2_CANCEL = 8;
  /// ENHANCE_YOUR_CALM.
  NGHTTP2_ENHANCE_YOUR_CALM = 11;

type
  /// size_t.
  nghttp2_size_t = NativeUInt;
  /// ssize_t.
  nghttp2_ssize_t = NativeInt;
  /// An opaque session.
  pnghttp2_session = Pointer;
  /// An opaque set of callbacks.
  pnghttp2_session_callbacks = Pointer;
  /// An opaque set of options.
  pnghttp2_option = Pointer;
  /// An opaque HPACK deflater.
  pnghttp2_hd_deflater = Pointer;
  /// An opaque HPACK inflater.
  pnghttp2_hd_inflater = Pointer;

  /// A header field.
  nghttp2_nv = record
    /// The name bytes (lower case in HTTP/2).
    name: PByte;
    /// The value bytes.
    value: PByte;
    /// The length of name.
    namelen: nghttp2_size_t;
    /// The length of value.
    valuelen: nghttp2_size_t;
    /// NGHTTP2_NV_FLAG_*.
    flags: Byte;
  end;
  /// A pointer to header fields.
  pnghttp2_nv = ^nghttp2_nv;

  /// The 9-byte frame head, as the library decodes it.
  nghttp2_frame_hd = record
    /// The payload length.
    length: nghttp2_size_t;
    /// The stream id (0 for the connection).
    stream_id: Integer;
    /// The frame type.
    type_: Byte;
    /// The frame flags.
    flags: Byte;
    /// Reserved, always 0.
    reserved: Byte;
  end;
  /// A pointer to a frame head; every frame starts with one.
  pnghttp2_frame_hd = ^nghttp2_frame_hd;

  /// A priority.
  nghttp2_priority_spec = record
    /// The stream it depends on.
    stream_id: Integer;
    /// The weight, 1 to 256.
    weight: Integer;
    /// Whether the dependency is exclusive.
    exclusive: Byte;
  end;

  /// A HEADERS frame as the callbacks see it.
  nghttp2_headers_frame = record
    /// The frame head.
    hd: nghttp2_frame_hd;
    /// The padding.
    padlen: nghttp2_size_t;
    /// The priority.
    pri_spec: nghttp2_priority_spec;
    /// The fields (only in a frame the library sends).
    nva: pnghttp2_nv;
    /// The number of fields.
    nvlen: nghttp2_size_t;
    /// NGHTTP2_HCAT_*.
    cat: Integer;
  end;
  /// A pointer to a HEADERS frame.
  pnghttp2_headers_frame = ^nghttp2_headers_frame;

  /// A GOAWAY frame.
  nghttp2_goaway_frame = record
    /// The frame head.
    hd: nghttp2_frame_hd;
    /// The last stream processed.
    last_stream_id: Integer;
    /// The error code.
    error_code: Cardinal;
    /// Debug data.
    opaque_data: PByte;
    /// Its length.
    opaque_data_len: nghttp2_size_t;
    /// Reserved.
    reserved: Byte;
  end;
  /// A pointer to a GOAWAY frame.
  pnghttp2_goaway_frame = ^nghttp2_goaway_frame;

  /// A RST_STREAM frame.
  nghttp2_rst_stream_frame = record
    /// The frame head.
    hd: nghttp2_frame_hd;
    /// The error code.
    error_code: Cardinal;
  end;
  /// A pointer to a RST_STREAM frame.
  pnghttp2_rst_stream_frame = ^nghttp2_rst_stream_frame;

  /// Where a data provider reads from.
  nghttp2_data_source = record
    case Integer of
      0: (fd: Integer);
      /// The data source as a pointer.
      1: (ptr: Pointer);
  end;
  /// A pointer to a data source.
  pnghttp2_data_source = ^nghttp2_data_source;

  /// Fills buf with up to length bytes of a stream's body; sets data_flags.
  nghttp2_data_source_read_callback = function(session: pnghttp2_session;
    stream_id: Integer; buf: PByte; length: nghttp2_size_t; data_flags: PCardinal;
    source: pnghttp2_data_source; user_data: Pointer): nghttp2_ssize_t; cdecl;

  /// The body of a response or request.
  nghttp2_data_provider = record
    /// Passed to read_callback.
    source: nghttp2_data_source;
    /// Reads the body.
    read_callback: nghttp2_data_source_read_callback;
  end;
  /// A pointer to a data provider.
  pnghttp2_data_provider = ^nghttp2_data_provider;

  /// One SETTINGS entry.
  nghttp2_settings_entry = record
    /// NGHTTP2_SETTINGS_*.
    settings_id: Integer;
    /// The value.
    value: Cardinal;
  end;
  /// A pointer to SETTINGS entries.
  pnghttp2_settings_entry = ^nghttp2_settings_entry;

  /// What nghttp2_version answers.
  nghttp2_info = record
    /// The age of the struct.
    age: Integer;
    /// The version as 0xMMmmpp.
    version_num: Integer;
    /// The version text.
    version_str: PAnsiChar;
    /// The protocol id ("h2").
    proto_str: PAnsiChar;
  end;
  /// A pointer to the version info.
  pnghttp2_info = ^nghttp2_info;

  /// A frame was received.
  nghttp2_on_frame_recv_callback = function(session: pnghttp2_session;
    frame: Pointer; user_data: Pointer): Integer; cdecl;
  /// A frame was sent.
  nghttp2_on_frame_send_callback = function(session: pnghttp2_session;
    frame: Pointer; user_data: Pointer): Integer; cdecl;
  /// A piece of DATA arrived.
  nghttp2_on_data_chunk_recv_callback = function(session: pnghttp2_session;
    flags: Byte; stream_id: Integer; data: PByte; len: nghttp2_size_t;
    user_data: Pointer): Integer; cdecl;
  /// A stream closed.
  nghttp2_on_stream_close_callback = function(session: pnghttp2_session;
    stream_id: Integer; error_code: Cardinal; user_data: Pointer): Integer; cdecl;
  /// A HEADERS frame starts.
  nghttp2_on_begin_headers_callback = function(session: pnghttp2_session;
    frame: Pointer; user_data: Pointer): Integer; cdecl;
  /// One header field arrived.
  nghttp2_on_header_callback = function(session: pnghttp2_session;
    frame: Pointer; name: PByte; namelen: nghttp2_size_t; value: PByte;
    valuelen: nghttp2_size_t; flags: Byte; user_data: Pointer): Integer; cdecl;
  /// A DATA frame with NO_COPY goes out: write framehd then length bytes of the source.
  nghttp2_send_data_callback = function(session: pnghttp2_session;
    frame: Pointer; framehd: PByte; length: nghttp2_size_t;
    source: pnghttp2_data_source; user_data: Pointer): Integer; cdecl;
  /// A frame could not be sent.
  nghttp2_on_frame_not_send_callback = function(session: pnghttp2_session;
    frame: Pointer; lib_error_code: Integer; user_data: Pointer): Integer; cdecl;
  /// The library reports an error with its text.
  nghttp2_error_callback2 = function(session: pnghttp2_session;
    lib_error_code: Integer; msg: PAnsiChar; len: nghttp2_size_t;
    user_data: Pointer): Integer; cdecl;

var
  /// Version info of the loaded library (least_version 0 always answers).
  nghttp2_version: function(least_version: Integer): pnghttp2_info; cdecl;
  /// The text of a library error code.
  nghttp2_strerror: function(lib_error_code: Integer): PAnsiChar; cdecl;
  /// The text of a protocol error code.
  nghttp2_http2_strerror: function(error_code: Cardinal): PAnsiChar; cdecl;

  /// Creates an empty set of callbacks.
  nghttp2_session_callbacks_new: function(out callbacks_ptr: pnghttp2_session_callbacks): Integer; cdecl;
  /// Frees a set of callbacks.
  nghttp2_session_callbacks_del: procedure(callbacks: pnghttp2_session_callbacks); cdecl;
  /// Sets on_frame_recv.
  nghttp2_session_callbacks_set_on_frame_recv_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_frame_recv_callback); cdecl;
  /// Sets on_frame_send.
  nghttp2_session_callbacks_set_on_frame_send_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_frame_send_callback); cdecl;
  /// Sets on_data_chunk_recv.
  nghttp2_session_callbacks_set_on_data_chunk_recv_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_data_chunk_recv_callback); cdecl;
  /// Sets on_stream_close.
  nghttp2_session_callbacks_set_on_stream_close_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_stream_close_callback); cdecl;
  /// Sets on_begin_headers.
  nghttp2_session_callbacks_set_on_begin_headers_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_begin_headers_callback); cdecl;
  /// Sets on_header.
  nghttp2_session_callbacks_set_on_header_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_header_callback); cdecl;
  /// Sets send_data (DATA with NO_COPY).
  nghttp2_session_callbacks_set_send_data_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_send_data_callback); cdecl;
  /// Sets on_frame_not_send.
  nghttp2_session_callbacks_set_on_frame_not_send_callback: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_on_frame_not_send_callback); cdecl;
  /// Sets error_callback2.
  nghttp2_session_callbacks_set_error_callback2: procedure(
    callbacks: pnghttp2_session_callbacks; cb: nghttp2_error_callback2); cdecl;

  /// Creates an empty set of options.
  nghttp2_option_new: function(out option_ptr: pnghttp2_option): Integer; cdecl;
  /// Frees a set of options.
  nghttp2_option_del: procedure(option: pnghttp2_option); cdecl;
  /// The most streams the peer may open before our SETTINGS is acknowledged.
  nghttp2_option_set_peer_max_concurrent_streams: procedure(option: pnghttp2_option; val: Cardinal); cdecl;
  /// The most bytes the header block of a frame may decode to (nil before 1.50).
  nghttp2_option_set_max_send_header_block_length: procedure(option: pnghttp2_option; val: nghttp2_size_t); cdecl;
  { How many RST_STREAM the peer may cause per second before the session ends
    (rapid reset, CVE-2023-44487; nil before 1.57). }
  nghttp2_option_set_stream_reset_rate_limit: procedure(option: pnghttp2_option;
    burst: UInt64; rate: UInt64); cdecl;
  { The most CONTINUATION frames per header block (CVE-2024-28182; nil before
    1.61). }
  nghttp2_option_set_max_continuations: procedure(option: pnghttp2_option; val: nghttp2_size_t); cdecl;
  /// The most SETTINGS entries a frame may carry (nil before 1.58).
  nghttp2_option_set_max_settings: procedure(option: pnghttp2_option; val: nghttp2_size_t); cdecl;

  /// A server session with options.
  nghttp2_session_server_new2: function(out session_ptr: pnghttp2_session;
    callbacks: pnghttp2_session_callbacks; user_data: Pointer;
    option: pnghttp2_option): Integer; cdecl;
  /// A client session with options.
  nghttp2_session_client_new2: function(out session_ptr: pnghttp2_session;
    callbacks: pnghttp2_session_callbacks; user_data: Pointer;
    option: pnghttp2_option): Integer; cdecl;
  /// Frees a session.
  nghttp2_session_del: procedure(session: pnghttp2_session); cdecl;
  /// Feeds received bytes; the bytes consumed, or a negative error.
  nghttp2_session_mem_recv: function(session: pnghttp2_session; data: PByte;
    datalen: nghttp2_size_t): nghttp2_ssize_t; cdecl;
  { Takes the next bytes to send: data_ptr points at them (valid until the
    next call); 0 when nothing is pending, negative on error. }
  nghttp2_session_mem_send: function(session: pnghttp2_session;
    out data_ptr: PByte): nghttp2_ssize_t; cdecl;
  /// Whether the session wants to read more.
  nghttp2_session_want_read: function(session: pnghttp2_session): Integer; cdecl;
  /// Whether the session has something to write.
  nghttp2_session_want_write: function(session: pnghttp2_session): Integer; cdecl;
  /// Resumes a stream whose data provider deferred.
  nghttp2_session_resume_data: function(session: pnghttp2_session; stream_id: Integer): Integer; cdecl;
  /// The user data of a stream.
  nghttp2_session_get_stream_user_data: function(session: pnghttp2_session;
    stream_id: Integer): Pointer; cdecl;
  /// Sets the user data of a stream.
  nghttp2_session_set_stream_user_data: function(session: pnghttp2_session;
    stream_id: Integer; stream_user_data: Pointer): Integer; cdecl;
  /// Ends the session with GOAWAY and error_code.
  nghttp2_session_terminate_session: function(session: pnghttp2_session;
    error_code: Cardinal): Integer; cdecl;
  /// The last stream id the peer opened.
  nghttp2_session_get_last_proc_stream_id: function(session: pnghttp2_session): Integer; cdecl;
  /// The window the peer gives the connection.
  nghttp2_session_get_remote_window_size: function(session: pnghttp2_session): Integer; cdecl;
  /// Sets the window we give the peer: the connection's (stream 0) or a stream's.
  nghttp2_session_set_local_window_size: function(session: pnghttp2_session; flags: Byte;
    stream_id: Integer; window_size: Integer): Integer; cdecl;
  /// Bytes received on a stream and not yet acknowledged with WINDOW_UPDATE.
  nghttp2_session_consume: function(session: pnghttp2_session; stream_id: Integer;
    size: nghttp2_size_t): Integer; cdecl;

  /// Sends SETTINGS.
  nghttp2_submit_settings: function(session: pnghttp2_session; flags: Byte;
    iv: pnghttp2_settings_entry; niv: nghttp2_size_t): Integer; cdecl;
  /// Answers a stream: HEADERS and a body from data_prd (nil: END_STREAM on the headers).
  nghttp2_submit_response: function(session: pnghttp2_session; stream_id: Integer;
    nva: pnghttp2_nv; nvlen: nghttp2_size_t; data_prd: pnghttp2_data_provider): Integer; cdecl;
  /// Sends trailers (HEADERS with END_STREAM) after the body.
  nghttp2_submit_trailer: function(session: pnghttp2_session; stream_id: Integer;
    nva: pnghttp2_nv; nvlen: nghttp2_size_t): Integer; cdecl;
  /// Sends a HEADERS frame (informational or trailers) with flags.
  nghttp2_submit_headers: function(session: pnghttp2_session; flags: Byte;
    stream_id: Integer; pri_spec: Pointer; nva: pnghttp2_nv; nvlen: nghttp2_size_t;
    stream_user_data: Pointer): Integer; cdecl;
  /// Opens a request stream; its id, or a negative error.
  nghttp2_submit_request: function(session: pnghttp2_session; pri_spec: Pointer;
    nva: pnghttp2_nv; nvlen: nghttp2_size_t; data_prd: pnghttp2_data_provider;
    stream_user_data: Pointer): Integer; cdecl;
  /// Resets a stream.
  nghttp2_submit_rst_stream: function(session: pnghttp2_session; flags: Byte;
    stream_id: Integer; error_code: Cardinal): Integer; cdecl;
  /// Sends PING (opaque_data: 8 bytes or nil).
  nghttp2_submit_ping: function(session: pnghttp2_session; flags: Byte;
    opaque_data: PByte): Integer; cdecl;
  /// Sends GOAWAY.
  nghttp2_submit_goaway: function(session: pnghttp2_session; flags: Byte;
    last_stream_id: Integer; error_code: Cardinal; opaque_data: PByte;
    opaque_data_len: nghttp2_size_t): Integer; cdecl;

  /// A new HPACK deflater with a dynamic table of at most max_deflate_dynamic_table_size.
  nghttp2_hd_deflate_new: function(out deflater_ptr: pnghttp2_hd_deflater;
    max_deflate_dynamic_table_size: nghttp2_size_t): Integer; cdecl;
  /// Frees a deflater.
  nghttp2_hd_deflate_del: procedure(deflater: pnghttp2_hd_deflater); cdecl;
  /// Encodes nva into buf; the length written or a negative error.
  nghttp2_hd_deflate_hd: function(deflater: pnghttp2_hd_deflater; buf: PByte;
    buflen: nghttp2_size_t; nva: pnghttp2_nv; nvlen: nghttp2_size_t): nghttp2_ssize_t; cdecl;
  /// The most bytes encoding nva may take.
  nghttp2_hd_deflate_bound: function(deflater: pnghttp2_hd_deflater;
    nva: pnghttp2_nv; nvlen: nghttp2_size_t): nghttp2_size_t; cdecl;
  /// A new HPACK inflater.
  nghttp2_hd_inflate_new: function(out inflater_ptr: pnghttp2_hd_inflater): Integer; cdecl;
  /// Frees an inflater.
  nghttp2_hd_inflate_del: procedure(inflater: pnghttp2_hd_inflater); cdecl;
  { Decodes one field of in_ (inlen bytes, in_final when it is the whole
    block): fills nv_out and sets inflate_flags; bytes consumed or negative. }
  nghttp2_hd_inflate_hd2: function(inflater: pnghttp2_hd_inflater; out nv_out: nghttp2_nv;
    out inflate_flags: Integer; in_: PByte; inlen: nghttp2_size_t;
    in_final: Integer): nghttp2_ssize_t; cdecl;
  /// Ends a header block.
  nghttp2_hd_inflate_end_headers: function(inflater: pnghttp2_hd_inflater): Integer; cdecl;
  /// The size of the inflater's dynamic table.
  nghttp2_hd_inflate_get_dynamic_table_size: function(inflater: pnghttp2_hd_inflater): nghttp2_size_t; cdecl;

const
  /// An inflate_flags bit: nv_out holds a field.
  NGHTTP2_HD_INFLATE_EMIT = $02;
  /// An inflate_flags bit: the header block ended.
  NGHTTP2_HD_INFLATE_FINAL = $01;

{ Loads the library: ALibrary, or the usual names of the platform. True when
  the library and every required symbol are there and it is 1.59 or newer;
  otherwise NGHTTP2LoadError says why. Safe to call again. }
function NGHTTP2Load(const ALibrary: string = ''): Boolean;
/// Whether the library is loaded.
function NGHTTP2IsLoaded: Boolean;
/// Why the last NGHTTP2Load failed.
function NGHTTP2LoadError: string;
/// The version of the loaded library as text ('' when none).
function NGHTTP2VersionStr: string;
/// The version of the loaded library as 0xMMmmpp (0 when none).
function NGHTTP2VersionNum: Integer;
/// Unloads the library and clears the entry points.
procedure NGHTTP2Unload;

implementation

type
  /// A handle of a loaded library.
  TNGHTTP2LibHandle = {$IFDEF FPC}TLibHandle{$ELSE}{$IFDEF MSWINDOWS}HMODULE{$ELSE}NativeUInt{$ENDIF}{$ENDIF};

var
  /// The loaded library, 0 when none.
  gLibrary: TNGHTTP2LibHandle = 0;
  /// Why the last load failed.
  gLoadError: string = '';

/// Opens a library file; 0 when it cannot.
function OpenLib(const AName: string): TNGHTTP2LibHandle;
begin
  {$IFDEF FPC}
  Result := LoadLibrary(AName);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result := LoadLibrary(PChar(AName));
  {$ELSE}
  Result := NativeUInt(dlopen(MarshaledAString(UTF8String(AName)), RTLD_LAZY));
  {$ENDIF}
  {$ENDIF}
end;

/// Closes a library.
procedure CloseLib(AHandle: TNGHTTP2LibHandle);
begin
  {$IFDEF FPC}
  FreeLibrary(AHandle);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  FreeLibrary(AHandle);
  {$ELSE}
  dlclose(AHandle);
  {$ENDIF}
  {$ENDIF}
end;

/// The address of a symbol, nil when missing.
function Sym(const AName: string): Pointer;
begin
  {$IFDEF FPC}
  Result := GetProcedureAddress(gLibrary, AName);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result := GetProcAddress(gLibrary, PChar(AName));
  {$ELSE}
  Result := dlsym(gLibrary, MarshaledAString(UTF8String(AName)));
  {$ENDIF}
  {$ENDIF}
end;

/// Binds a required symbol; False (and the error) when missing.
function Need(var AProc; const AName: string): Boolean;
begin
  Pointer(AProc) := Sym(AName);
  Result := Pointer(AProc) <> nil;
  if not Result then
    gLoadError := 'nghttp2: symbol ' + AName + ' missing';
end;

/// Sets every loaded entry point to nil.
procedure ClearEntries;
begin
  @nghttp2_version := nil;
  @nghttp2_strerror := nil;
  @nghttp2_http2_strerror := nil;
  @nghttp2_session_callbacks_new := nil;
  @nghttp2_session_callbacks_del := nil;
  @nghttp2_session_callbacks_set_on_frame_recv_callback := nil;
  @nghttp2_session_callbacks_set_on_frame_send_callback := nil;
  @nghttp2_session_callbacks_set_on_data_chunk_recv_callback := nil;
  @nghttp2_session_callbacks_set_on_stream_close_callback := nil;
  @nghttp2_session_callbacks_set_on_begin_headers_callback := nil;
  @nghttp2_session_callbacks_set_on_header_callback := nil;
  @nghttp2_session_callbacks_set_send_data_callback := nil;
  @nghttp2_session_callbacks_set_on_frame_not_send_callback := nil;
  @nghttp2_session_callbacks_set_error_callback2 := nil;
  @nghttp2_option_new := nil;
  @nghttp2_option_del := nil;
  @nghttp2_option_set_peer_max_concurrent_streams := nil;
  @nghttp2_option_set_max_send_header_block_length := nil;
  @nghttp2_option_set_stream_reset_rate_limit := nil;
  @nghttp2_option_set_max_continuations := nil;
  @nghttp2_option_set_max_settings := nil;
  @nghttp2_session_server_new2 := nil;
  @nghttp2_session_client_new2 := nil;
  @nghttp2_session_del := nil;
  @nghttp2_session_mem_recv := nil;
  @nghttp2_session_mem_send := nil;
  @nghttp2_session_want_read := nil;
  @nghttp2_session_want_write := nil;
  @nghttp2_session_resume_data := nil;
  @nghttp2_session_get_stream_user_data := nil;
  @nghttp2_session_set_stream_user_data := nil;
  @nghttp2_session_terminate_session := nil;
  @nghttp2_session_get_last_proc_stream_id := nil;
  @nghttp2_session_get_remote_window_size := nil;
  @nghttp2_session_consume := nil;
  @nghttp2_session_set_local_window_size := nil;
  @nghttp2_submit_settings := nil;
  @nghttp2_submit_response := nil;
  @nghttp2_submit_trailer := nil;
  @nghttp2_submit_headers := nil;
  @nghttp2_submit_request := nil;
  @nghttp2_submit_rst_stream := nil;
  @nghttp2_submit_ping := nil;
  @nghttp2_submit_goaway := nil;
  @nghttp2_hd_deflate_new := nil;
  @nghttp2_hd_deflate_del := nil;
  @nghttp2_hd_deflate_hd := nil;
  @nghttp2_hd_deflate_bound := nil;
  @nghttp2_hd_inflate_new := nil;
  @nghttp2_hd_inflate_del := nil;
  @nghttp2_hd_inflate_hd2 := nil;
  @nghttp2_hd_inflate_end_headers := nil;
  @nghttp2_hd_inflate_get_dynamic_table_size := nil;
end;

/// Binds every entry point; False at the first required one missing.
function BindAll: Boolean;
begin
  Result := Need(@nghttp2_version, 'nghttp2_version') and
    Need(@nghttp2_strerror, 'nghttp2_strerror') and
    Need(@nghttp2_http2_strerror, 'nghttp2_http2_strerror') and
    Need(@nghttp2_session_callbacks_new, 'nghttp2_session_callbacks_new') and
    Need(@nghttp2_session_callbacks_del, 'nghttp2_session_callbacks_del') and
    Need(@nghttp2_session_callbacks_set_on_frame_recv_callback, 'nghttp2_session_callbacks_set_on_frame_recv_callback') and
    Need(@nghttp2_session_callbacks_set_on_frame_send_callback, 'nghttp2_session_callbacks_set_on_frame_send_callback') and
    Need(@nghttp2_session_callbacks_set_on_data_chunk_recv_callback, 'nghttp2_session_callbacks_set_on_data_chunk_recv_callback') and
    Need(@nghttp2_session_callbacks_set_on_stream_close_callback, 'nghttp2_session_callbacks_set_on_stream_close_callback') and
    Need(@nghttp2_session_callbacks_set_on_begin_headers_callback, 'nghttp2_session_callbacks_set_on_begin_headers_callback') and
    Need(@nghttp2_session_callbacks_set_on_header_callback, 'nghttp2_session_callbacks_set_on_header_callback') and
    Need(@nghttp2_session_callbacks_set_send_data_callback, 'nghttp2_session_callbacks_set_send_data_callback') and
    Need(@nghttp2_session_callbacks_set_on_frame_not_send_callback, 'nghttp2_session_callbacks_set_on_frame_not_send_callback') and
    Need(@nghttp2_session_callbacks_set_error_callback2, 'nghttp2_session_callbacks_set_error_callback2') and
    Need(@nghttp2_option_new, 'nghttp2_option_new') and
    Need(@nghttp2_option_del, 'nghttp2_option_del') and
    Need(@nghttp2_option_set_peer_max_concurrent_streams, 'nghttp2_option_set_peer_max_concurrent_streams') and
    Need(@nghttp2_session_server_new2, 'nghttp2_session_server_new2') and
    Need(@nghttp2_session_client_new2, 'nghttp2_session_client_new2') and
    Need(@nghttp2_session_del, 'nghttp2_session_del') and
    Need(@nghttp2_session_mem_recv, 'nghttp2_session_mem_recv') and
    Need(@nghttp2_session_mem_send, 'nghttp2_session_mem_send') and
    Need(@nghttp2_session_want_read, 'nghttp2_session_want_read') and
    Need(@nghttp2_session_want_write, 'nghttp2_session_want_write') and
    Need(@nghttp2_session_resume_data, 'nghttp2_session_resume_data') and
    Need(@nghttp2_session_get_stream_user_data, 'nghttp2_session_get_stream_user_data') and
    Need(@nghttp2_session_set_stream_user_data, 'nghttp2_session_set_stream_user_data') and
    Need(@nghttp2_session_terminate_session, 'nghttp2_session_terminate_session') and
    Need(@nghttp2_session_get_last_proc_stream_id, 'nghttp2_session_get_last_proc_stream_id') and
    Need(@nghttp2_session_get_remote_window_size, 'nghttp2_session_get_remote_window_size') and
    Need(@nghttp2_session_consume, 'nghttp2_session_consume') and
    Need(@nghttp2_session_set_local_window_size, 'nghttp2_session_set_local_window_size') and
    Need(@nghttp2_submit_settings, 'nghttp2_submit_settings') and
    Need(@nghttp2_submit_response, 'nghttp2_submit_response') and
    Need(@nghttp2_submit_trailer, 'nghttp2_submit_trailer') and
    Need(@nghttp2_submit_headers, 'nghttp2_submit_headers') and
    Need(@nghttp2_submit_request, 'nghttp2_submit_request') and
    Need(@nghttp2_submit_rst_stream, 'nghttp2_submit_rst_stream') and
    Need(@nghttp2_submit_ping, 'nghttp2_submit_ping') and
    Need(@nghttp2_submit_goaway, 'nghttp2_submit_goaway') and
    Need(@nghttp2_hd_deflate_new, 'nghttp2_hd_deflate_new') and
    Need(@nghttp2_hd_deflate_del, 'nghttp2_hd_deflate_del') and
    Need(@nghttp2_hd_deflate_hd, 'nghttp2_hd_deflate_hd') and
    Need(@nghttp2_hd_deflate_bound, 'nghttp2_hd_deflate_bound') and
    Need(@nghttp2_hd_inflate_new, 'nghttp2_hd_inflate_new') and
    Need(@nghttp2_hd_inflate_del, 'nghttp2_hd_inflate_del') and
    Need(@nghttp2_hd_inflate_hd2, 'nghttp2_hd_inflate_hd2') and
    Need(@nghttp2_hd_inflate_end_headers, 'nghttp2_hd_inflate_end_headers') and
    Need(@nghttp2_hd_inflate_get_dynamic_table_size, 'nghttp2_hd_inflate_get_dynamic_table_size');
  if not Result then
    Exit;
  // newer options: nil on an older library
  @nghttp2_option_set_max_send_header_block_length := Sym('nghttp2_option_set_max_send_header_block_length');
  @nghttp2_option_set_stream_reset_rate_limit := Sym('nghttp2_option_set_stream_reset_rate_limit');
  @nghttp2_option_set_max_continuations := Sym('nghttp2_option_set_max_continuations');
  @nghttp2_option_set_max_settings := Sym('nghttp2_option_set_max_settings');
end;

function NGHTTP2Load(const ALibrary: string): Boolean;
const
  {$IFDEF MSWINDOWS}
  cNames: array[0..2] of string = ('libnghttp2-14.dll', 'nghttp2.dll', 'libnghttp2.dll');
  {$ELSE}
  {$IFDEF DARWIN}
  cNames: array[0..2] of string = ('libnghttp2.14.dylib', 'libnghttp2.dylib', '/opt/homebrew/lib/libnghttp2.dylib');
  {$ELSE}
  cNames: array[0..2] of string = ('libnghttp2.so.14', 'libnghttp2.so', 'libnghttp2.so.14.0');
  {$ENDIF}
  {$ENDIF}
var
  vInt: Integer;
  vInfo: pnghttp2_info;
begin
  Result := gLibrary <> 0;
  if Result then
    Exit;
  gLoadError := '';
  if ALibrary <> '' then
    gLibrary := OpenLib(ALibrary)
  else
    for vInt := Low(cNames) to High(cNames) do
    begin
      gLibrary := OpenLib(cNames[vInt]);
      if gLibrary <> 0 then
        Break;
    end;
  if gLibrary = 0 then
  begin
    gLoadError := 'nghttp2: the library was not found';
    if ALibrary <> '' then
      gLoadError := gLoadError + ' (' + ALibrary + ')';
    Exit;
  end;
  if not BindAll then
  begin
    NGHTTP2Unload;
    Exit;
  end;
  vInfo := nghttp2_version(0);
  if (vInfo = nil) or (vInfo^.version_num < NGHTTP2_MIN_VERSION) then
  begin
    if vInfo <> nil then
      gLoadError := 'nghttp2: version ' + string(AnsiString(vInfo^.version_str)) +
        ' is older than 1.59'
    else
      gLoadError := 'nghttp2: no version';
    NGHTTP2Unload;
    Exit;
  end;
  Result := True;
end;

function NGHTTP2IsLoaded: Boolean;
begin
  Result := gLibrary <> 0;
end;

function NGHTTP2LoadError: string;
begin
  Result := gLoadError;
end;

procedure NGHTTP2Unload;
begin
  if gLibrary <> 0 then
    CloseLib(gLibrary);
  gLibrary := 0;
  ClearEntries;
end;

function NGHTTP2VersionNum: Integer;
begin
  if gLibrary = 0 then
    Result := 0
  else
    Result := nghttp2_version(0)^.version_num;
end;

function NGHTTP2VersionStr: string;
begin
  if gLibrary = 0 then
    Result := ''
  else
    Result := string(AnsiString(nghttp2_version(0)^.version_str));
end;

end.
