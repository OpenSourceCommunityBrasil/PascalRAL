{ Object Pascal binding for the MsQuic library (Microsoft QUIC), API version 2.

  Translated against src/inc/msquic.h of the MsQuic 2.6.1 release
  (github.com/microsoft/msquic). Status codes are per platform - HRESULTs on
  Windows, errno values on Linux, as msquic_winuser.h and msquic_posix.h
  define them - and so is QUIC_FAILED: on Windows failure is the high bit, on
  POSIX it is any positive value, with PENDING and CONTINUE negative. Apple's
  errno numbering differs from Linux and is not translated, so MsQuicLoad
  refuses there instead of misreading every status.

  Three things decide whether this binding is correct:

  1. Calling convention is cdecl. msquic.h defines QUIC_API as __cdecl. It is
     irrelevant on x64, where there is only one convention, and it is not on
     Win32.
  2. Record layout follows msquic.h field by field, and it is the same on
     every ABI the library ships for - MSVC on Windows, AAPCS on Android ARM32
     and ARM64, which were checked with offsetof against the 2.6.1 header.
     The two records the library READS from us, QUIC_SETTINGS and QUIC_ADDR,
     spell their padding out, because MSVC aligns a 64-bit field on 8 bytes
     even on 32-bit targets while the i386 System V ABI that FPC follows
     aligns it on 4. They are not packed, though: the C side may read a field
     with an instruction that requires its natural alignment, and a packed
     record on the stack is a SIGBUS on ARM32 the day it lands one byte off.
     The event records are packed - the library hands them over by pointer
     and Pascal only reads them, which it does safely at any address.
  3. The event unions are raw bytes plus typed accessors. A Pascal variant
     record would leave the offset of the variant part to the compiler; here
     every variant that matters is its own small record whose layout can be
     read off the C header line by line.

  The API itself arrives as a table of function pointers from
  MsQuicOpenVersion, so there is exactly one imported symbol and everything
  else is a field of QUIC_API_TABLE. }
unit MsQuic;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}
  {$PACKRECORDS C}
{$ENDIF}
{$IFNDEF FPC}
  {$IF CompilerVersion > 23}
    {$LEGACYIFEND ON}
  {$IFEND}
  {$ALIGN 8}
{$ENDIF}
// The two alignment directives above pin the C layout whatever the project
// including this unit chose - see point 2.

interface

uses
  SysUtils
  {$IFDEF FPC}
  , DynLibs
  {$ELSE}
  {$IFDEF MSWINDOWS}, Windows{$ELSE}, Posix.Dlfcn{$ENDIF}
  {$ENDIF};

// Pointer width, which decides where one of the event unions begins. Delphi
// defines CPUX64/CPUARM64, FPC defines CPU64.
{$IFDEF CPUX64}{$DEFINE QUIC_PTR64}{$ENDIF}
{$IFDEF CPUARM64}{$DEFINE QUIC_PTR64}{$ENDIF}
{$IFDEF CPU64}{$DEFINE QUIC_PTR64}{$ENDIF}

const
{$IFDEF MSWINDOWS}
  /// File name of the msquic library on this platform.
  MSQUIC_LIBRARY = 'msquic.dll';
{$ELSE}
{$IFDEF ANDROID}
  // Android has no versioned sonames: the packager only carries lib/<abi>/*.so
  // into the APK, so a file called libmsquic.so.2 never reaches the device and
  // the plain name is what dlopen resolves, in the application's own library
  // folder. Both compilers define ANDROID for that target. The build RAL
  // publishes needs Android 9 (API 28): msquic's OpenSSL glue calls glob(),
  // which the NDK only declares from there, so it is built against API 28,
  // and the symbol versions that build records (getentropy@LIBC_P) make the
  // dynamic linker refuse it on an older device - MsQuicLoad passes the
  // linker's own message on.

  /// File name of the msquic library on this platform.
  MSQUIC_LIBRARY = 'libmsquic.so';
{$ELSE}
  /// File name of the msquic library on this platform.
  MSQUIC_LIBRARY = 'libmsquic.so.2';
{$ENDIF}
{$ENDIF}

  /// The API version MsQuicOpenVersion is asked for.
  QUIC_API_VERSION_2 = 2;

  /// Longest ALPN, in bytes.
  QUIC_MAX_ALPN_LENGTH = 255;
  /// Longest server name, in bytes.
  QUIC_MAX_SNI_LENGTH  = 65535;

type
  /// Handle of any msquic object.
  HQUIC  = Pointer;
  /// Pointer to an HQUIC.
  PHQUIC = ^HQUIC;

  { HRESULT on Windows. Failure is the high bit, which is why QUIC_FAILED
    tests the bit instead of comparing with zero - on POSIX MsQuic uses errno
    values and the test is a different one. }
  QUIC_STATUS = Cardinal;
  /// Pointer to a QUIC_STATUS.
  PQUIC_STATUS = ^QUIC_STATUS;

  /// A QUIC variable-length integer (62 bits).
  QUIC_UINT62 = UInt64;
  /// ADDRESS_FAMILY on Windows, i.e. USHORT.
  QUIC_ADDRESS_FAMILY = Word;

const
{$IFDEF MSWINDOWS}
  // msquic_winuser.h: HRESULTs, failure is the high bit

  /// Status: success.
  QUIC_STATUS_SUCCESS               = QUIC_STATUS($00000000);
  /// Status: the operation completes later, through a callback.
  QUIC_STATUS_PENDING               = QUIC_STATUS($000703E5);
  /// Status: the operation continues.
  QUIC_STATUS_CONTINUE              = QUIC_STATUS($000704DE);
  /// Status: out of memory.
  QUIC_STATUS_OUT_OF_MEMORY         = QUIC_STATUS($8007000E);
  /// Status: a parameter is invalid.
  QUIC_STATUS_INVALID_PARAMETER     = QUIC_STATUS($80070057);
  /// Status: the object is in the wrong state for the call.
  QUIC_STATUS_INVALID_STATE         = QUIC_STATUS($8007139F);
  /// Status: not supported.
  QUIC_STATUS_NOT_SUPPORTED         = QUIC_STATUS($80004002);
  /// Status: not found.
  QUIC_STATUS_NOT_FOUND             = QUIC_STATUS($80070490);
  /// Status: a file was not found.
  QUIC_STATUS_FILE_NOT_FOUND        = QUIC_STATUS($80070002);
  /// Status: the buffer is too small.
  QUIC_STATUS_BUFFER_TOO_SMALL      = QUIC_STATUS($8007007A);
  /// Status: the handshake failed.
  QUIC_STATUS_HANDSHAKE_FAILURE     = QUIC_STATUS($80410000);
  /// Status: the operation was aborted.
  QUIC_STATUS_ABORTED               = QUIC_STATUS($80004004);
  /// Status: the address is in use.
  QUIC_STATUS_ADDRESS_IN_USE        = QUIC_STATUS($80072740);
  /// Status: the address is invalid.
  QUIC_STATUS_INVALID_ADDRESS       = QUIC_STATUS($80072741);
  /// Status: the connection timed out.
  QUIC_STATUS_CONNECTION_TIMEOUT    = QUIC_STATUS($80410006);
  /// Status: the connection closed for being idle.
  QUIC_STATUS_CONNECTION_IDLE       = QUIC_STATUS($80410005);
  /// Status: the peer is unreachable.
  QUIC_STATUS_UNREACHABLE           = QUIC_STATUS($800704D0);
  /// Status: internal error.
  QUIC_STATUS_INTERNAL_ERROR        = QUIC_STATUS($80410003);
  /// Status: the peer refused the connection.
  QUIC_STATUS_CONNECTION_REFUSED    = QUIC_STATUS($800704C9);
  /// Status: protocol error.
  QUIC_STATUS_PROTOCOL_ERROR        = QUIC_STATUS($80410004);
  /// Status: no QUIC version in common.
  QUIC_STATUS_VER_NEG_ERROR         = QUIC_STATUS($80410001);
  /// Status: TLS error.
  QUIC_STATUS_TLS_ERROR             = QUIC_STATUS($80072B18);
  /// Status: the application cancelled the operation.
  QUIC_STATUS_USER_CANCELED         = QUIC_STATUS($80410002);
  /// Status: no ALPN in common.
  QUIC_STATUS_ALPN_NEG_FAILURE      = QUIC_STATUS($80410007);
  /// Status: the stream limit was reached.
  QUIC_STATUS_STREAM_LIMIT_REACHED  = QUIC_STATUS($80410008);
  /// Status: another listener already uses the ALPN.
  QUIC_STATUS_ALPN_IN_USE           = QUIC_STATUS($80410009);
  /// Status: the certificate expired.
  QUIC_STATUS_CERT_EXPIRED          = QUIC_STATUS($800B0101);
  /// Status: the certificate chains to an untrusted root.
  QUIC_STATUS_CERT_UNTRUSTED_ROOT   = QUIC_STATUS($800B0109);
  /// Status: no certificate.
  QUIC_STATUS_CERT_NO_CERT          = QUIC_STATUS($8009030E);
  { QUIC_STATUS_TLS_ALERT(42): what a callback returns to refuse a peer's
    certificate. The alerts occupy $80410100..$804101FF. }
  QUIC_STATUS_BAD_CERTIFICATE       = QUIC_STATUS($8041012A);
  /// Status: base of the TLS alert statuses (plus the alert code).
  QUIC_STATUS_TLS_ALERT_BASE        = QUIC_STATUS($80410100);
{$ELSE}
  // msquic_posix.h on Linux: errno values, failure is any positive value.
  // Apple numbers most of these differently; MsQuicLoad refuses there.

  /// Status: success.
  QUIC_STATUS_SUCCESS               = QUIC_STATUS(0);
  /// Status: the operation completes later, through a callback.
  QUIC_STATUS_PENDING               = QUIC_STATUS($FFFFFFFE); // -2
  /// Status: the operation continues.
  QUIC_STATUS_CONTINUE              = QUIC_STATUS($FFFFFFFF); // -1
  /// Status: out of memory.
  QUIC_STATUS_OUT_OF_MEMORY         = QUIC_STATUS(12);       // ENOMEM
  /// Status: a parameter is invalid.
  QUIC_STATUS_INVALID_PARAMETER     = QUIC_STATUS(22);       // EINVAL
  /// Status: the object is in the wrong state for the call.
  QUIC_STATUS_INVALID_STATE         = QUIC_STATUS(1);        // EPERM
  /// Status: not supported.
  QUIC_STATUS_NOT_SUPPORTED         = QUIC_STATUS(95);       // EOPNOTSUPP
  /// Status: not found.
  QUIC_STATUS_NOT_FOUND             = QUIC_STATUS(2);        // ENOENT
  /// Status: a file was not found.
  QUIC_STATUS_FILE_NOT_FOUND        = QUIC_STATUS(2);        // ENOENT
  /// Status: the buffer is too small.
  QUIC_STATUS_BUFFER_TOO_SMALL      = QUIC_STATUS(75);       // EOVERFLOW
  /// Status: the handshake failed.
  QUIC_STATUS_HANDSHAKE_FAILURE     = QUIC_STATUS(103);      // ECONNABORTED
  /// Status: the operation was aborted.
  QUIC_STATUS_ABORTED               = QUIC_STATUS(125);      // ECANCELED
  /// Status: the address is in use.
  QUIC_STATUS_ADDRESS_IN_USE        = QUIC_STATUS(98);       // EADDRINUSE
  /// Status: the address is invalid.
  QUIC_STATUS_INVALID_ADDRESS       = QUIC_STATUS(97);       // EAFNOSUPPORT
  /// Status: the connection timed out.
  QUIC_STATUS_CONNECTION_TIMEOUT    = QUIC_STATUS(110);      // ETIMEDOUT
  /// Status: the connection closed for being idle.
  QUIC_STATUS_CONNECTION_IDLE       = QUIC_STATUS(62);       // ETIME
  /// Status: the peer is unreachable.
  QUIC_STATUS_UNREACHABLE           = QUIC_STATUS(113);      // EHOSTUNREACH
  /// Status: internal error.
  QUIC_STATUS_INTERNAL_ERROR        = QUIC_STATUS(5);        // EIO
  /// Status: the peer refused the connection.
  QUIC_STATUS_CONNECTION_REFUSED    = QUIC_STATUS(111);      // ECONNREFUSED
  /// Status: protocol error.
  QUIC_STATUS_PROTOCOL_ERROR        = QUIC_STATUS(71);       // EPROTO
  /// Status: no QUIC version in common.
  QUIC_STATUS_VER_NEG_ERROR         = QUIC_STATUS(93);       // EPROTONOSUPPORT
  /// Status: TLS error.
  QUIC_STATUS_TLS_ERROR             = QUIC_STATUS(126);      // ENOKEY
  /// Status: the application cancelled the operation.
  QUIC_STATUS_USER_CANCELED         = QUIC_STATUS(130);      // EOWNERDEAD
  /// Status: no ALPN in common.
  QUIC_STATUS_ALPN_NEG_FAILURE      = QUIC_STATUS(92);       // ENOPROTOOPT
  /// Status: the stream limit was reached.
  QUIC_STATUS_STREAM_LIMIT_REACHED  = QUIC_STATUS(86);       // ESTRPIPE
  /// Status: another listener already uses the ALPN.
  QUIC_STATUS_ALPN_IN_USE           = QUIC_STATUS(91);       // EPROTOTYPE
  // ERROR_BASE = 200000000 ($BEBC200), TLS_ERROR_BASE = +256, CERT_ERROR_BASE
  // = +512: QUIC_STATUS_TLS_ALERT(n) = $BEBC300 + n, QUIC_STATUS_CERT_ERROR(n)
  // = $BEBC400 + n. Nothing is defined at ERROR_BASE itself.

  /// Status: the certificate expired.
  QUIC_STATUS_CERT_EXPIRED          = QUIC_STATUS($0BEBC401);
  /// Status: the certificate chains to an untrusted root.
  QUIC_STATUS_CERT_UNTRUSTED_ROOT   = QUIC_STATUS($0BEBC402);
  /// Status: no certificate.
  QUIC_STATUS_CERT_NO_CERT          = QUIC_STATUS($0BEBC403);
  /// Status: the certificate was refused.
  QUIC_STATUS_BAD_CERTIFICATE       = QUIC_STATUS($0BEBC32A);
  /// Status: base of the TLS alert statuses (plus the alert code).
  QUIC_STATUS_TLS_ALERT_BASE        = QUIC_STATUS($0BEBC300);
{$ENDIF}

  // QUIC_EXECUTION_PROFILE

  /// Execution profile: low latency (the default).
  QUIC_EXECUTION_PROFILE_LOW_LATENCY         = 0;
  /// Execution profile: maximum throughput.
  QUIC_EXECUTION_PROFILE_TYPE_MAX_THROUGHPUT = 1;
  /// Execution profile: background traffic.
  QUIC_EXECUTION_PROFILE_TYPE_SCAVENGER      = 2;
  /// Execution profile: real time.
  QUIC_EXECUTION_PROFILE_TYPE_REAL_TIME      = 3;

  // QUIC_CREDENTIAL_TYPE

  /// Credential: none (a client without a certificate).
  QUIC_CREDENTIAL_TYPE_NONE                       = 0;
  /// Credential: a certificate hash in the Windows store (QUIC_CERTIFICATE_HASH).
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_HASH           = 1;
  /// Credential: a certificate hash in a named Windows store.
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_HASH_STORE     = 2;
  /// Credential: a Windows certificate context.
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_CONTEXT        = 3;
  /// Credential: certificate and key files (QUIC_CERTIFICATE_FILE).
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE           = 4;
  /// Credential: certificate and encrypted key files (QUIC_CERTIFICATE_FILE_PROTECTED).
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE_PROTECTED = 5;
  /// Credential: a PKCS#12 blob in memory (QUIC_CERTIFICATE_PKCS12).
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_PKCS12         = 6;

  // QUIC_CREDENTIAL_FLAGS

  /// Credential flags: none.
  QUIC_CREDENTIAL_FLAG_NONE                          = $00000000;
  /// Credential flag: the configuration is a client one.
  QUIC_CREDENTIAL_FLAG_CLIENT                        = $00000001;
  /// Credential flag: load the credential asynchronously.
  QUIC_CREDENTIAL_FLAG_LOAD_ASYNCHRONOUS             = $00000002;
  /// Credential flag: do not validate the peer certificate.
  QUIC_CREDENTIAL_FLAG_NO_CERTIFICATE_VALIDATION     = $00000004;
  /// Credential flag: raise QUIC_CONNECTION_EVENT_PEER_CERTIFICATE_RECEIVED.
  QUIC_CREDENTIAL_FLAG_INDICATE_CERTIFICATE_RECEIVED = $00000010;
  /// Credential flag: hand the validation result to the application to decide.
  QUIC_CREDENTIAL_FLAG_DEFER_CERTIFICATE_VALIDATION  = $00000020;
  /// Credential flag: the server asks for a client certificate.
  QUIC_CREDENTIAL_FLAG_REQUIRE_CLIENT_AUTHENTICATION = $00000040;
  { OpenSSL builds only, and outside Windows the only validation there is:
    without it msquic asks its platform layer, which on POSIX is a stub that
    refuses every certificate - and, under DEFER_CERTIFICATE_VALIDATION,
    reports SUCCESS as the deferred verdict. With it, OpenSSL checks the
    chain and the host name against its store. }
  QUIC_CREDENTIAL_FLAG_USE_TLS_BUILTIN_CERTIFICATE_VALIDATION = $00000080;
  /// Credential flag: AllowedCipherSuites is set.
  QUIC_CREDENTIAL_FLAG_SET_ALLOWED_CIPHER_SUITES     = $00002000;
  /// Credential flag: certificates are given to the application in portable (DER) form.
  QUIC_CREDENTIAL_FLAG_USE_PORTABLE_CERTIFICATES     = $00004000;
  /// Credential flag: CaCertificateFile is set (OpenSSL build).
  QUIC_CREDENTIAL_FLAG_SET_CA_CERTIFICATE_FILE       = $00100000;

  // QUIC_SERVER_RESUMPTION_LEVEL

  /// Server resumption: none.
  QUIC_SERVER_NO_RESUME          = 0;
  /// Server resumption: resume, no 0-RTT.
  QUIC_SERVER_RESUME_ONLY        = 1;
  /// Server resumption: resume and accept 0-RTT.
  QUIC_SERVER_RESUME_AND_ZERORTT = 2;

  // QUIC_CONNECTION_SHUTDOWN_FLAGS

  /// Shutdown flags: none.
  QUIC_CONNECTION_SHUTDOWN_FLAG_NONE   = $0000;
  /// Shutdown flag: close without telling the peer.
  QUIC_CONNECTION_SHUTDOWN_FLAG_SILENT = $0001;

  // QUIC_STREAM_OPEN_FLAGS

  /// Stream open flags: none (bidirectional).
  QUIC_STREAM_OPEN_FLAG_NONE           = $0000;
  /// Stream open flag: unidirectional.
  QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL = $0001;
  /// Stream open flag: may be sent in 0-RTT.
  QUIC_STREAM_OPEN_FLAG_0_RTT          = $0002;

  // QUIC_STREAM_START_FLAGS

  /// Stream start flags: none.
  QUIC_STREAM_START_FLAG_NONE             = $0000;
  /// Stream start flag: tell the peer at once.
  QUIC_STREAM_START_FLAG_IMMEDIATE        = $0001;
  /// Stream start flag: fail instead of waiting for stream credit.
  QUIC_STREAM_START_FLAG_FAIL_BLOCKED     = $0002;
  /// Stream start flag: shut the stream down when the start fails.
  QUIC_STREAM_START_FLAG_SHUTDOWN_ON_FAIL = $0004;

  // QUIC_STREAM_SHUTDOWN_FLAGS

  /// Stream shutdown flags: none.
  QUIC_STREAM_SHUTDOWN_FLAG_NONE          = $0000;
  /// Stream shutdown flag: end the send side after the data queued.
  QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL      = $0001;
  /// Stream shutdown flag: abort the send side.
  QUIC_STREAM_SHUTDOWN_FLAG_ABORT_SEND    = $0002;
  /// Stream shutdown flag: abort the receive side.
  QUIC_STREAM_SHUTDOWN_FLAG_ABORT_RECEIVE = $0004;
  /// Stream shutdown flag: abort both sides.
  QUIC_STREAM_SHUTDOWN_FLAG_ABORT         = $0006;
  /// Stream shutdown flag: complete the shutdown at once.
  QUIC_STREAM_SHUTDOWN_FLAG_IMMEDIATE     = $0008;

  // QUIC_RECEIVE_FLAGS

  /// Receive flags: none.
  QUIC_RECEIVE_FLAG_NONE  = $0000;
  /// Receive flag: the data came in 0-RTT.
  QUIC_RECEIVE_FLAG_0_RTT = $0001;
  /// Receive flag: the peer ended its send side.
  QUIC_RECEIVE_FLAG_FIN   = $0002;

  // QUIC_SEND_FLAGS

  /// Send flags: none.
  QUIC_SEND_FLAG_NONE        = $0000;
  /// Send flag: may go in 0-RTT.
  QUIC_SEND_FLAG_ALLOW_0_RTT = $0001;
  /// Send flag: start the stream with this send.
  QUIC_SEND_FLAG_START       = $0002;
  /// Send flag: end the send side after this data.
  QUIC_SEND_FLAG_FIN         = $0004;
  /// Send flag: more sends follow; wait before sending.
  QUIC_SEND_FLAG_DELAY_SEND  = $0010;

  // QUIC_LISTENER_EVENT_TYPE

  /// Listener event: a new connection arrived.
  QUIC_LISTENER_EVENT_NEW_CONNECTION   = 0;
  /// Listener event: the listener stopped.
  QUIC_LISTENER_EVENT_STOP_COMPLETE    = 1;
  /// Listener event: denial-of-service mode changed.
  QUIC_LISTENER_EVENT_DOS_MODE_CHANGED = 2;

  // QUIC_CONNECTION_EVENT_TYPE

  /// Connection event: the handshake completed.
  QUIC_CONNECTION_EVENT_CONNECTED                       = 0;
  /// Connection event: the transport started the shutdown.
  QUIC_CONNECTION_EVENT_SHUTDOWN_INITIATED_BY_TRANSPORT = 1;
  /// Connection event: the peer started the shutdown.
  QUIC_CONNECTION_EVENT_SHUTDOWN_INITIATED_BY_PEER      = 2;
  /// Connection event: the shutdown completed.
  QUIC_CONNECTION_EVENT_SHUTDOWN_COMPLETE               = 3;
  /// Connection event: the local address changed.
  QUIC_CONNECTION_EVENT_LOCAL_ADDRESS_CHANGED           = 4;
  /// Connection event: the peer address changed.
  QUIC_CONNECTION_EVENT_PEER_ADDRESS_CHANGED            = 5;
  /// Connection event: the peer opened a stream.
  QUIC_CONNECTION_EVENT_PEER_STREAM_STARTED             = 6;
  /// Connection event: the peer allows more streams.
  QUIC_CONNECTION_EVENT_STREAMS_AVAILABLE               = 7;
  /// Connection event: the peer wants more streams.
  QUIC_CONNECTION_EVENT_PEER_NEEDS_STREAMS              = 8;
  /// Connection event: the ideal processor changed.
  QUIC_CONNECTION_EVENT_IDEAL_PROCESSOR_CHANGED         = 9;
  /// Connection event: datagram support changed.
  QUIC_CONNECTION_EVENT_DATAGRAM_STATE_CHANGED          = 10;
  /// Connection event: a datagram arrived.
  QUIC_CONNECTION_EVENT_DATAGRAM_RECEIVED               = 11;
  /// Connection event: a datagram send changed state.
  QUIC_CONNECTION_EVENT_DATAGRAM_SEND_STATE_CHANGED     = 12;
  /// Connection event: the session was resumed.
  QUIC_CONNECTION_EVENT_RESUMED                         = 13;
  /// Connection event: a resumption ticket arrived.
  QUIC_CONNECTION_EVENT_RESUMPTION_TICKET_RECEIVED      = 14;
  /// Connection event: the peer certificate arrived.
  QUIC_CONNECTION_EVENT_PEER_CERTIFICATE_RECEIVED       = 15;

  // QUIC_STREAM_EVENT_TYPE

  /// Stream event: the start completed.
  QUIC_STREAM_EVENT_START_COMPLETE         = 0;
  /// Stream event: data arrived.
  QUIC_STREAM_EVENT_RECEIVE                = 1;
  /// Stream event: a send completed.
  QUIC_STREAM_EVENT_SEND_COMPLETE          = 2;
  /// Stream event: the peer ended its send side.
  QUIC_STREAM_EVENT_PEER_SEND_SHUTDOWN     = 3;
  /// Stream event: the peer aborted its send side.
  QUIC_STREAM_EVENT_PEER_SEND_ABORTED      = 4;
  /// Stream event: the peer aborted its receive side.
  QUIC_STREAM_EVENT_PEER_RECEIVE_ABORTED   = 5;
  /// Stream event: the send side shut down.
  QUIC_STREAM_EVENT_SEND_SHUTDOWN_COMPLETE = 6;
  /// Stream event: the stream shut down.
  QUIC_STREAM_EVENT_SHUTDOWN_COMPLETE      = 7;
  /// Stream event: the ideal send buffer size changed.
  QUIC_STREAM_EVENT_IDEAL_SEND_BUFFER_SIZE = 8;
  /// Stream event: the peer accepted the stream.
  QUIC_STREAM_EVENT_PEER_ACCEPTED          = 9;
  /// Stream event: data was lost and the stream may cancel.
  QUIC_STREAM_EVENT_CANCEL_ON_LOSS         = 10;

  // Address families: AF_INET6 is 23 on Windows and 10 on Linux.

  /// Address family unspecified (either).
  QUIC_ADDRESS_FAMILY_UNSPEC = 0;
  /// Address family IPv4.
  QUIC_ADDRESS_FAMILY_INET   = 2;
  /// Address family IPv6.
  QUIC_ADDRESS_FAMILY_INET6  = {$IFDEF MSWINDOWS}23{$ELSE}10{$ENDIF};

  // QUIC_PARAM_*

  /// Global parameter: the library version (four numbers).
  QUIC_PARAM_GLOBAL_LIBRARY_VERSION  = $01000004;
  /// Global parameter: the git hash of the library build.
  QUIC_PARAM_GLOBAL_LIBRARY_GIT_HASH = $01000008;
  /// Global parameter: the TLS provider (SChannel or OpenSSL).
  QUIC_PARAM_GLOBAL_TLS_PROVIDER     = $0100000A;
  /// Configuration parameter: its QUIC_SETTINGS.
  QUIC_PARAM_CONFIGURATION_SETTINGS  = $03000000;
  /// Listener parameter: the address it listens on.
  QUIC_PARAM_LISTENER_LOCAL_ADDRESS  = $04000000;
  /// Connection parameter: the QUIC version in use.
  QUIC_PARAM_CONN_QUIC_VERSION       = $05000000;
  /// Connection parameter: the local address.
  QUIC_PARAM_CONN_LOCAL_ADDRESS      = $05000001;
  /// Connection parameter: the peer address.
  QUIC_PARAM_CONN_REMOTE_ADDRESS     = $05000002;
  /// Connection parameter: its QUIC_SETTINGS.
  QUIC_PARAM_CONN_SETTINGS           = $05000004;
  /// TLS parameter: the ALPN negotiated.
  QUIC_PARAM_TLS_NEGOTIATED_ALPN     = $06000001;
  /// Stream parameter: the stream ID.
  QUIC_PARAM_STREAM_ID               = $08000000;

  // Bit positions of QUIC_SETTINGS.IsSetFlags, in header order.

  /// IsSetFlags bit: bytes per key before a key update.
  QUIC_SETTING_MaxBytesPerKey             = UInt64(1) shl 0;
  /// IsSetFlags bit: handshake idle timeout, ms.
  QUIC_SETTING_HandshakeIdleTimeoutMs     = UInt64(1) shl 1;
  /// IsSetFlags bit: idle timeout, ms.
  QUIC_SETTING_IdleTimeoutMs              = UInt64(1) shl 2;
  /// IsSetFlags bit: default stream receive window.
  QUIC_SETTING_StreamRecvWindowDefault    = UInt64(1) shl 6;
  /// IsSetFlags bit: default stream receive buffer.
  QUIC_SETTING_StreamRecvBufferDefault    = UInt64(1) shl 7;
  /// IsSetFlags bit: connection flow-control window.
  QUIC_SETTING_ConnFlowControlWindow      = UInt64(1) shl 8;
  /// IsSetFlags bit: initial congestion window, in packets.
  QUIC_SETTING_InitialWindowPackets       = UInt64(1) shl 11;
  /// IsSetFlags bit: send idle timeout, ms.
  QUIC_SETTING_SendIdleTimeoutMs          = UInt64(1) shl 12;
  /// IsSetFlags bit: initial RTT estimate, ms.
  QUIC_SETTING_InitialRttMs               = UInt64(1) shl 13;
  /// IsSetFlags bit: maximum ACK delay, ms.
  QUIC_SETTING_MaxAckDelayMs              = UInt64(1) shl 14;
  /// IsSetFlags bit: disconnect timeout, ms.
  QUIC_SETTING_DisconnectTimeoutMs        = UInt64(1) shl 15;
  /// IsSetFlags bit: keep-alive interval, ms.
  QUIC_SETTING_KeepAliveIntervalMs        = UInt64(1) shl 16;
  /// IsSetFlags bit: congestion control algorithm.
  QUIC_SETTING_CongestionControlAlgorithm = UInt64(1) shl 17;
  /// IsSetFlags bit: bidirectional streams the peer may open.
  QUIC_SETTING_PeerBidiStreamCount        = UInt64(1) shl 18;
  /// IsSetFlags bit: unidirectional streams the peer may open.
  QUIC_SETTING_PeerUnidiStreamCount       = UInt64(1) shl 19;
  /// IsSetFlags bit: minimum MTU.
  QUIC_SETTING_MinimumMtu                 = UInt64(1) shl 22;
  /// IsSetFlags bit: maximum MTU.
  QUIC_SETTING_MaximumMtu                 = UInt64(1) shl 23;
  /// IsSetFlags bit: send buffering.
  QUIC_SETTING_SendBufferingEnabled       = UInt64(1) shl 24;
  /// IsSetFlags bit: pacing.
  QUIC_SETTING_PacingEnabled              = UInt64(1) shl 25;
  /// IsSetFlags bit: connection migration.
  QUIC_SETTING_MigrationEnabled           = UInt64(1) shl 26;
  /// IsSetFlags bit: datagram receive.
  QUIC_SETTING_DatagramReceiveEnabled     = UInt64(1) shl 27;
  /// IsSetFlags bit: server resumption level.
  QUIC_SETTING_ServerResumptionLevel      = UInt64(1) shl 28;
  /// IsSetFlags bit: greasing of the QUIC bit.
  QUIC_SETTING_GreaseQuicBitEnabled       = UInt64(1) shl 32;
  /// IsSetFlags bit: ECN.
  QUIC_SETTING_EcnEnabled                 = UInt64(1) shl 33;

  // Bits of QUIC_SETTINGS.BitFlags, LSB first as MSVC packs them.

  /// BitFlags bit: send buffering.
  QUIC_SETTINGS_BIT_SendBufferingEnabled   = $01;
  /// BitFlags bit: pacing.
  QUIC_SETTINGS_BIT_PacingEnabled          = $02;
  /// BitFlags bit: connection migration.
  QUIC_SETTINGS_BIT_MigrationEnabled       = $04;
  /// BitFlags bit: datagram receive.
  QUIC_SETTINGS_BIT_DatagramReceiveEnabled = $08;
  /// Shift of ServerResumptionLevel (2 bits) in QUIC_SETTINGS.BitFlags.
  QUIC_SETTINGS_SHIFT_ServerResumptionLevel = 4; // 2 bits wide
  /// BitFlags bit: greasing of the QUIC bit.
  QUIC_SETTINGS_BIT_GreaseQuicBitEnabled   = $40;
  /// BitFlags bit: ECN.
  QUIC_SETTINGS_BIT_EcnEnabled             = $80;

  { sizeof(QUIC_SETTINGS) as msquic.h lays it out, the same on every ABI the
    library ships for. Checked at load time, because a silent mismatch here
    misconfigures the connection instead of failing. }
  QUIC_SETTINGS_SIZE = 144;

type
  { SOCKADDR_INET on Windows, the sockaddr_in/sockaddr_in6 union on POSIX:
    28 bytes and the same offsets either way. Every field already sits at its
    natural offset, and the record keeps the 4-byte alignment the library
    reads it with - see point 2 on why it is not packed. }
  QUIC_ADDR = record
    case Integer of
      0: (si_family: Word);
      /// The IPv4 view: family, port and address in network order.
      1: (v4_family: Word; v4_port: Word; v4_addr: Cardinal;
          v4_zero: array[0..7] of Byte);
      /// The IPv6 view: family, port, flow info, address and scope.
      2: (v6_family: Word; v6_port: Word; v6_flowinfo: Cardinal;
          v6_addr: array[0..15] of Byte; v6_scope_id: Cardinal);
  end;
  /// Pointer to a QUIC_ADDR.
  PQUIC_ADDR = ^QUIC_ADDR;

  { uint32 followed by a pointer. No 64-bit field, so the natural layout of
    both Pascal compilers already matches MSVC. }
  QUIC_BUFFER = record
    /// Bytes at Buffer.
    Length: Cardinal;
    /// The data.
    Buffer: PByte;
  end;
  /// Pointer to a QUIC_BUFFER.
  PQUIC_BUFFER = ^QUIC_BUFFER;
  /// An array view of the buffers of a receive event.
  TQuicBufferArray = array[0..1023] of QUIC_BUFFER;
  /// Pointer to a TQuicBufferArray.
  PQuicBufferArray = ^TQuicBufferArray;

  /// Parameters of RegistrationOpen.
  QUIC_REGISTRATION_CONFIG = record
    /// Name of the application.
    AppName: PAnsiChar;
    /// One of the QUIC_EXECUTION_PROFILE_* values.
    ExecutionProfile: Cardinal;
  end;
  /// Pointer to a QUIC_REGISTRATION_CONFIG.
  PQUIC_REGISTRATION_CONFIG = ^QUIC_REGISTRATION_CONFIG;

  /// Certificate and key file names.
  QUIC_CERTIFICATE_FILE = record
    /// Private key file (PEM).
    PrivateKeyFile: PAnsiChar;
    /// Certificate file (PEM).
    CertificateFile: PAnsiChar;
  end;
  /// Pointer to a QUIC_CERTIFICATE_FILE.
  PQUIC_CERTIFICATE_FILE = ^QUIC_CERTIFICATE_FILE;

  { QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE_PROTECTED: the same two files plus
    the password of an encrypted private key. }
  QUIC_CERTIFICATE_FILE_PROTECTED = record
    /// Encrypted private key file (PEM).
    PrivateKeyFile: PAnsiChar;
    /// Certificate file (PEM).
    CertificateFile: PAnsiChar;
    /// Password of the key.
    PrivateKeyPassword: PAnsiChar;
  end;
  /// Pointer to a QUIC_CERTIFICATE_FILE_PROTECTED.
  PQUIC_CERTIFICATE_FILE_PROTECTED = ^QUIC_CERTIFICATE_FILE_PROTECTED;

  /// A PKCS#12 blob and its password.
  QUIC_CERTIFICATE_PKCS12 = record
    /// The PKCS#12 bytes.
    Asn1Blob: PByte;
    /// Bytes at Asn1Blob.
    Asn1BlobLength: Cardinal;
    /// Password of the blob, or nil.
    PrivateKeyPassword: PAnsiChar;
  end;
  /// Pointer to a QUIC_CERTIFICATE_PKCS12.
  PQUIC_CERTIFICATE_PKCS12 = ^QUIC_CERTIFICATE_PKCS12;

  /// The SHA-1 thumbprint of a certificate in the Windows store.
  QUIC_CERTIFICATE_HASH = record
    /// The SHA-1 thumbprint.
    ShaHash: array[0..19] of Byte;
  end;
  /// Pointer to a QUIC_CERTIFICATE_HASH.
  PQUIC_CERTIFICATE_HASH = ^QUIC_CERTIFICATE_HASH;

  { The C union is a single pointer, so it is declared as one; the caller
    casts whichever certificate record matches CredType into CertificateRef. }
  QUIC_CREDENTIAL_CONFIG = record
    /// One of the QUIC_CREDENTIAL_TYPE_* values.
    CredType: Cardinal;
    /// QUIC_CREDENTIAL_FLAG_* values.
    Flags: Cardinal;
    /// The credential CredType names (a QUIC_CERTIFICATE_* record).
    CertificateRef: Pointer;
    /// Principal name of a Windows store credential.
    Principal: PAnsiChar;
    /// Reserved; nil.
    Reserved: Pointer;
    /// Completion handler of an asynchronous load.
    AsyncHandler: Pointer;
    /// Allowed TLS cipher suites, with QUIC_CREDENTIAL_FLAG_SET_ALLOWED_CIPHER_SUITES.
    AllowedCipherSuites: Cardinal;
    { CA file that replaces the system store, with
      QUIC_CREDENTIAL_FLAG_SET_CA_CERTIFICATE_FILE. }
    CaCertificateFile: PAnsiChar;
  end;
  /// Pointer to a QUIC_CREDENTIAL_CONFIG.
  PQUIC_CREDENTIAL_CONFIG = ^QUIC_CREDENTIAL_CONFIG;

  /// What a listener learns of a new connection.
  QUIC_NEW_CONNECTION_INFO = record
    /// The QUIC version.
    QuicVersion: Cardinal;
    /// The local address.
    LocalAddress: PQUIC_ADDR;
    /// The peer address.
    RemoteAddress: PQUIC_ADDR;
    /// Bytes at CryptoBuffer.
    CryptoBufferLength: Cardinal;
    /// Bytes at ClientAlpnList.
    ClientAlpnListLength: Word;
    /// Bytes at ServerName.
    ServerNameLength: Word;
    /// Bytes at NegotiatedAlpn.
    NegotiatedAlpnLength: Byte;
    /// The TLS ClientHello.
    CryptoBuffer: PByte;
    /// The ALPN list the client offered (TLS wire format).
    ClientAlpnList: PByte;
    /// The ALPN chosen.
    NegotiatedAlpn: PByte;
    /// The SNI name, not null-terminated.
    ServerName: PAnsiChar;
  end;
  /// Pointer to a QUIC_NEW_CONNECTION_INFO.
  PQUIC_NEW_CONNECTION_INFO = ^QUIC_NEW_CONNECTION_INFO;

  { Explicit padding, so every compiler puts each field at the header's
    offset - it mixes 64-, 32-, 16- and 8-bit fields - and no packing, so the
    record keeps the 8-byte alignment the library assumes when it reads the
    64-bit ones (point 2). The offsets in the comments are what msquic
    expects, on 32 and 64 bits alike. }
  QUIC_SETTINGS = record
    /// Which fields are set: the QUIC_SETTING_* bits.
    IsSetFlags: UInt64;                          //   0
    /// Bytes per key before a key update.
    MaxBytesPerKey: UInt64;                      //   8
    /// Handshake idle timeout, ms.
    HandshakeIdleTimeoutMs: UInt64;              //  16
    /// Idle timeout, ms.
    IdleTimeoutMs: UInt64;                       //  24
    /// MTU discovery search timeout, us.
    MtuDiscoverySearchCompleteTimeoutUs: UInt64; //  32
    /// TLS client send buffer limit.
    TlsClientMaxSendBuffer: Cardinal;            //  40
    /// TLS server send buffer limit.
    TlsServerMaxSendBuffer: Cardinal;            //  44
    /// Default stream receive window.
    StreamRecvWindowDefault: Cardinal;           //  48
    /// Default stream receive buffer.
    StreamRecvBufferDefault: Cardinal;           //  52
    /// Connection flow-control window.
    ConnFlowControlWindow: Cardinal;             //  56
    /// Maximum worker queue delay, us.
    MaxWorkerQueueDelayUs: Cardinal;             //  60
    /// Stateless operations per worker.
    MaxStatelessOperations: Cardinal;            //  64
    /// Initial congestion window, in packets.
    InitialWindowPackets: Cardinal;              //  68
    /// Send idle timeout, ms.
    SendIdleTimeoutMs: Cardinal;                 //  72
    /// Initial RTT estimate, ms.
    InitialRttMs: Cardinal;                      //  76
    /// Maximum ACK delay, ms.
    MaxAckDelayMs: Cardinal;                     //  80
    /// Disconnect timeout, ms.
    DisconnectTimeoutMs: Cardinal;               //  84
    /// Keep-alive interval, ms.
    KeepAliveIntervalMs: Cardinal;               //  88
    /// Congestion control algorithm.
    CongestionControlAlgorithm: Word;            //  92
    /// Bidirectional streams the peer may open.
    PeerBidiStreamCount: Word;                   //  94
    /// Unidirectional streams the peer may open.
    PeerUnidiStreamCount: Word;                  //  96
    /// Stateless operations per binding.
    MaxBindingStatelessOperations: Word;         //  98
    /// Stateless operation expiry, ms.
    StatelessOperationExpirationMs: Word;        // 100
    /// Minimum MTU.
    MinimumMtu: Word;                            // 102
    /// Maximum MTU.
    MaximumMtu: Word;                            // 104
    /// The boolean settings: the QUIC_SETTINGS_BIT_* bits and ServerResumptionLevel.
    BitFlags: Byte;                              // 106
    /// Operations per drain.
    MaxOperationsPerDrain: Byte;                 // 107
    /// Missing MTU probes before giving up.
    MtuDiscoveryMissingProbeCount: Byte;         // 108
    /// Padding up to the next 32-bit field.
    Pad0: array[0..2] of Byte;                   // 109 - up to the uint32
    /// Idle time before the destination CID changes, ms.
    DestCidUpdateIdleTimeoutMs: Cardinal;        // 112
    /// Padding up to the next 64-bit field.
    Pad1: Cardinal;                              // 116 - up to the uint64
    /// Further boolean settings (bit field).
    Flags: UInt64;                               // 120
    /// Receive window of locally opened bidirectional streams.
    StreamRecvWindowBidiLocalDefault: Cardinal;  // 128
    /// Receive window of peer-opened bidirectional streams.
    StreamRecvWindowBidiRemoteDefault: Cardinal; // 132
    /// Receive window of unidirectional streams.
    StreamRecvWindowUnidiDefault: Cardinal;      // 136
    /// Tail padding; the record is 144 bytes.
    Pad2: Cardinal;                              // 140 - tail padding
  end;                                           // 144
  /// Pointer to a QUIC_SETTINGS.
  PQUIC_SETTINGS = ^QUIC_SETTINGS;

  //
  // Events. Every event is a 32-bit type followed by a union, and where that
  // union begins is decided by the widest alignment among its members. The
  // union itself is raw bytes here, read through the typed records below.
  //
  // The connection and stream unions both contain members with a 64-bit field
  // (QUIC_UINT62 error codes, the receive offsets), so their alignment is 8 and
  // the union starts at offset 8 on 32- and 64-bit alike - MSVC aligns a 64-bit
  // field on 8 bytes on x86 too, and so does AAPCS on ARM32.
  //
  // QUIC_LISTENER_EVENT is the exception, and it is the one that catches
  // everybody: its members are two pointers and two BOOLEAN bitfields, with no
  // 64-bit field anywhere, so its alignment is the pointer's. The union starts
  // at offset 8 on 64-bit and at offset 4 on 32-bit. Assuming 8 everywhere
  // compiles and runs fine on x64 and, on x86, reads Info and Connection four
  // bytes past where they are: ConnectionSetConfiguration then gets a garbage
  // handle and every incoming connection is refused, with the listener happily
  // bound to the port.
  //

  /// A listener event: its type and the union of its data.
  QUIC_LISTENER_EVENT = packed record
    /// One of the QUIC_LISTENER_EVENT_* values.
    EventType: Cardinal;
    /// Alignment of Data on 64-bit targets.
    {$IFDEF QUIC_PTR64}Pad: Cardinal;{$ENDIF}
    /// The data of the event; read it through the TQuic*Data records.
    Data: array[0..63] of Byte;
  end;
  /// Pointer to a QUIC_LISTENER_EVENT.
  PQUIC_LISTENER_EVENT = ^QUIC_LISTENER_EVENT;

  /// A connection event: its type and the union of its data.
  QUIC_CONNECTION_EVENT = packed record
    /// One of the QUIC_CONNECTION_EVENT_* values.
    EventType: Cardinal;
    /// Alignment of Data.
    Pad: Cardinal;
    /// The data of the event; read it through the TQuic*Data records.
    Data: array[0..95] of Byte;
  end;
  /// Pointer to a QUIC_CONNECTION_EVENT.
  PQUIC_CONNECTION_EVENT = ^QUIC_CONNECTION_EVENT;

  /// A stream event: its type and the union of its data.
  QUIC_STREAM_EVENT = packed record
    /// One of the QUIC_STREAM_EVENT_* values.
    EventType: Cardinal;
    /// Alignment of Data.
    Pad: Cardinal;
    /// The data of the event; read it through the TQuic*Data records.
    Data: array[0..63] of Byte;
  end;
  /// Pointer to a QUIC_STREAM_EVENT.
  PQUIC_STREAM_EVENT = ^QUIC_STREAM_EVENT;

  /// QUIC_LISTENER_EVENT.NEW_CONNECTION
  TQuicNewConnectionData = packed record
    /// What is known of the connection.
    Info: PQUIC_NEW_CONNECTION_INFO;
    /// The new connection; set its callback before returning.
    Connection: HQUIC;
  end;
  /// Pointer to a TQuicNewConnectionData.
  PQuicNewConnectionData = ^TQuicNewConnectionData;

  /// QUIC_CONNECTION_EVENT.CONNECTED
  TQuicConnectedData = packed record
    /// Whether a session was resumed.
    SessionResumed: ByteBool;
    /// Bytes at NegotiatedAlpn.
    NegotiatedAlpnLength: Byte;
    /// Alignment of NegotiatedAlpn.
    Pad: array[0..SizeOf(Pointer) - 3] of Byte;
    /// The ALPN negotiated.
    NegotiatedAlpn: PByte;
  end;
  /// Pointer to a TQuicConnectedData.
  PQuicConnectedData = ^TQuicConnectedData;

  /// QUIC_CONNECTION_EVENT.SHUTDOWN_INITIATED_BY_TRANSPORT
  TQuicShutdownByTransportData = packed record
    /// Why the transport closed.
    Status: QUIC_STATUS;
    /// Alignment of ErrorCode.
    Pad: Cardinal;
    /// The QUIC error code sent or received.
    ErrorCode: QUIC_UINT62;
  end;
  /// Pointer to a TQuicShutdownByTransportData.
  PQuicShutdownByTransportData = ^TQuicShutdownByTransportData;

  /// QUIC_CONNECTION_EVENT.SHUTDOWN_INITIATED_BY_PEER
  TQuicShutdownByPeerData = packed record
    /// The application error code of the peer.
    ErrorCode: QUIC_UINT62;
  end;
  /// Pointer to a TQuicShutdownByPeerData.
  PQuicShutdownByPeerData = ^TQuicShutdownByPeerData;

  /// QUIC_CONNECTION_EVENT.PEER_STREAM_STARTED
  TQuicPeerStreamStartedData = packed record
    /// The stream; set its callback before returning.
    Stream: HQUIC;
    /// QUIC_STREAM_OPEN_FLAG_* values.
    Flags: Cardinal;
  end;
  /// Pointer to a TQuicPeerStreamStartedData.
  PQuicPeerStreamStartedData = ^TQuicPeerStreamStartedData;

  { QUIC_CONNECTION_EVENT.PEER_CERTIFICATE_RECEIVED, indicated only with
    QUIC_CREDENTIAL_FLAG_INDICATE_CERTIFICATE_RECEIVED. With
    QUIC_CREDENTIAL_FLAG_USE_PORTABLE_CERTIFICATES the two pointers are
    QUIC_BUFFERs: the DER of the peer's certificate, and the PKCS#7 DER of
    its chain. With DEFER_CERTIFICATE_VALIDATION the library's own verdict
    arrives in DeferredStatus (SUCCESS when it trusted the certificate) and
    the callback's return value is the final decision. }
  TQuicPeerCertificateReceivedData = packed record
    /// The peer certificate (platform form, or DER when portable).
    Certificate: Pointer;
    /// Validation error flags (SChannel).
    DeferredErrorFlags: Cardinal;
    /// The validation result msquic deferred.
    DeferredStatus: QUIC_STATUS;
    /// The certificate chain.
    Chain: Pointer;
  end;
  /// Pointer to a TQuicPeerCertificateReceivedData.
  PQuicPeerCertificateReceivedData = ^TQuicPeerCertificateReceivedData;

  /// QUIC_STREAM_EVENT.RECEIVE
  TQuicReceiveData = packed record
    /// Offset of the data in the stream.
    AbsoluteOffset: UInt64;
    /// Bytes in all the buffers.
    TotalBufferLength: UInt64;
    /// The buffers.
    Buffers: PQuicBufferArray;
    /// Number of buffers.
    BufferCount: Cardinal;
    /// QUIC_RECEIVE_FLAG_* values.
    Flags: Cardinal;
  end;
  /// Pointer to a TQuicReceiveData.
  PQuicReceiveData = ^TQuicReceiveData;

  /// QUIC_STREAM_EVENT.SEND_COMPLETE
  TQuicSendCompleteData = packed record
    /// Whether the send was cancelled.
    Canceled: ByteBool;
    /// Alignment of ClientContext.
    Pad: array[0..SizeOf(Pointer) - 2] of Byte;
    /// The context given to StreamSend.
    ClientContext: Pointer;
  end;
  /// Pointer to a TQuicSendCompleteData.
  PQuicSendCompleteData = ^TQuicSendCompleteData;

  /// QUIC_STREAM_EVENT.START_COMPLETE
  TQuicStartCompleteData = packed record
    /// Result of the start.
    Status: QUIC_STATUS;
    /// Alignment of ID.
    Pad: Cardinal;
    /// The stream ID.
    ID: QUIC_UINT62;
    /// Whether the peer accepted the stream (bit 0).
    PeerAccepted: Byte;
  end;
  /// Pointer to a TQuicStartCompleteData.
  PQuicStartCompleteData = ^TQuicStartCompleteData;

  /// Callback of a listener.
  QUIC_LISTENER_CALLBACK = function(Listener: HQUIC; Context: Pointer;
    Event: PQUIC_LISTENER_EVENT): QUIC_STATUS; cdecl;
  /// Callback of a connection.
  QUIC_CONNECTION_CALLBACK = function(Connection: HQUIC; Context: Pointer;
    Event: PQUIC_CONNECTION_EVENT): QUIC_STATUS; cdecl;
  /// Callback of a stream.
  QUIC_STREAM_CALLBACK = function(Stream: HQUIC; Context: Pointer;
    Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl;

  { The API function table returned by MsQuicOpenVersion. Declared up to
    ConnectionOpenInPartition (MsQuic 2.5); the preview-only entries that
    follow are simply not described here, which is safe because the record
    only ever describes a prefix of a table the library owns. }
  QUIC_API_TABLE = record
    /// Sets the context of a handle.
    SetContext: procedure(Handle: HQUIC; Context: Pointer); cdecl;
    /// The context of a handle.
    GetContext: function(Handle: HQUIC): Pointer; cdecl;
    /// Sets the callback and context of a handle.
    SetCallbackHandler: procedure(Handle: HQUIC; Handler: Pointer;
      Context: Pointer); cdecl;

    /// Sets a parameter of a handle.
    SetParam: function(Handle: HQUIC; Param: Cardinal; BufferLength: Cardinal;
      Buffer: Pointer): QUIC_STATUS; cdecl;
    /// Reads a parameter of a handle.
    GetParam: function(Handle: HQUIC; Param: Cardinal; BufferLength: PCardinal;
      Buffer: Pointer): QUIC_STATUS; cdecl;

    /// Opens a registration.
    RegistrationOpen: function(Config: PQUIC_REGISTRATION_CONFIG;
      out Registration: HQUIC): QUIC_STATUS; cdecl;
    /// Closes a registration.
    RegistrationClose: procedure(Registration: HQUIC); cdecl;
    /// Shuts down every connection of a registration.
    RegistrationShutdown: procedure(Registration: HQUIC; Flags: Cardinal;
      ErrorCode: QUIC_UINT62); cdecl;

    /// Opens a configuration (ALPNs and settings).
    ConfigurationOpen: function(Registration: HQUIC;
      const AlpnBuffers: PQUIC_BUFFER; AlpnBufferCount: Cardinal;
      Settings: PQUIC_SETTINGS; SettingsSize: Cardinal; Context: Pointer;
      out Configuration: HQUIC): QUIC_STATUS; cdecl;
    /// Closes a configuration.
    ConfigurationClose: procedure(Configuration: HQUIC); cdecl;
    /// Loads the credential of a configuration.
    ConfigurationLoadCredential: function(Configuration: HQUIC;
      CredConfig: PQUIC_CREDENTIAL_CONFIG): QUIC_STATUS; cdecl;

    /// Opens a listener.
    ListenerOpen: function(Registration: HQUIC; Handler: QUIC_LISTENER_CALLBACK;
      Context: Pointer; out Listener: HQUIC): QUIC_STATUS; cdecl;
    /// Closes a listener.
    ListenerClose: procedure(Listener: HQUIC); cdecl;
    /// Starts listening on an address.
    ListenerStart: function(Listener: HQUIC; const AlpnBuffers: PQUIC_BUFFER;
      AlpnBufferCount: Cardinal; LocalAddress: PQUIC_ADDR): QUIC_STATUS; cdecl;
    /// Stops a listener.
    ListenerStop: procedure(Listener: HQUIC); cdecl;

    /// Opens a client connection.
    ConnectionOpen: function(Registration: HQUIC;
      Handler: QUIC_CONNECTION_CALLBACK; Context: Pointer;
      out Connection: HQUIC): QUIC_STATUS; cdecl;
    /// Closes a connection.
    ConnectionClose: procedure(Connection: HQUIC); cdecl;
    /// Starts the shutdown of a connection.
    ConnectionShutdown: procedure(Connection: HQUIC; Flags: Cardinal;
      ErrorCode: QUIC_UINT62); cdecl;
    /// Connects to a server.
    ConnectionStart: function(Connection: HQUIC; Configuration: HQUIC;
      Family: QUIC_ADDRESS_FAMILY; ServerName: PAnsiChar;
      ServerPort: Word): QUIC_STATUS; cdecl;
    /// Sets the configuration of an accepted connection.
    ConnectionSetConfiguration: function(Connection: HQUIC;
      Configuration: HQUIC): QUIC_STATUS; cdecl;
    /// Sends a resumption ticket.
    ConnectionSendResumptionTicket: function(Connection: HQUIC; Flags: Cardinal;
      DataLength: Word; ResumptionData: PByte): QUIC_STATUS; cdecl;

    /// Opens a stream.
    StreamOpen: function(Connection: HQUIC; Flags: Cardinal;
      Handler: QUIC_STREAM_CALLBACK; Context: Pointer;
      out Stream: HQUIC): QUIC_STATUS; cdecl;
    /// Closes a stream.
    StreamClose: procedure(Stream: HQUIC); cdecl;
    /// Starts a stream.
    StreamStart: function(Stream: HQUIC; Flags: Cardinal): QUIC_STATUS; cdecl;
    /// Shuts a stream down.
    StreamShutdown: function(Stream: HQUIC; Flags: Cardinal;
      ErrorCode: QUIC_UINT62): QUIC_STATUS; cdecl;
    /// Queues buffers on a stream.
    StreamSend: function(Stream: HQUIC; const Buffers: PQUIC_BUFFER;
      BufferCount: Cardinal; Flags: Cardinal;
      ClientSendContext: Pointer): QUIC_STATUS; cdecl;
    /// Tells how many received bytes the application consumed.
    StreamReceiveComplete: procedure(Stream: HQUIC; BufferLength: UInt64); cdecl;
    /// Turns receive callbacks on or off.
    StreamReceiveSetEnabled: function(Stream: HQUIC;
      IsEnabled: ByteBool): QUIC_STATUS; cdecl;

    /// Sends a datagram.
    DatagramSend: function(Connection: HQUIC; const Buffers: PQUIC_BUFFER;
      BufferCount: Cardinal; Flags: Cardinal;
      ClientSendContext: Pointer): QUIC_STATUS; cdecl;

    /// Completes an asynchronous resumption ticket validation.
    ConnectionResumptionTicketValidationComplete: function(Connection: HQUIC;
      AResult: ByteBool): QUIC_STATUS; cdecl;
    /// Completes a deferred certificate validation.
    ConnectionCertificateValidationComplete: function(Connection: HQUIC;
      AResult: ByteBool; TlsAlert: Cardinal): QUIC_STATUS; cdecl;

    /// Opens a client connection in a given partition.
    ConnectionOpenInPartition: function(Registration: HQUIC;
      PartitionIndex: Word; Handler: QUIC_CONNECTION_CALLBACK; Context: Pointer;
      out Connection: HQUIC): QUIC_STATUS; cdecl;
  end;
  /// Pointer to a QUIC_API_TABLE.
  PQUIC_API_TABLE = ^QUIC_API_TABLE;

var
  /// Valid between a successful MsQuicLoad and MsQuicUnload.
  MsQuicApi: PQUIC_API_TABLE = nil;

/// Whether AStatus is a failure.
function QUIC_FAILED(const AStatus: QUIC_STATUS): Boolean;
/// Whether AStatus is a success.
function QUIC_SUCCEEDED(const AStatus: QUIC_STATUS): Boolean;
{ Whether a status is about the peer's certificate - a TLS alert or one of
  the CERT_* codes - as opposed to the network or the library. }
function QuicStatusIsCertError(const AStatus: QUIC_STATUS): Boolean;
{ Text of a QUIC_ADDR: dotted IPv4, or IPv6 with its longest zero run
  collapsed the way RFC 5952 writes it. An IPv4-mapped IPv6 address comes
  back as the IPv4 it carries, which is what a dual-stack listener reports
  for a v4 peer. APort receives the port in host order. }
function QuicAddrToStr(const AAddr: QUIC_ADDR; out APort: Word): string;

{ Loads msquic and fills MsQuicApi. Returns QUIC_STATUS_SUCCESS, or the status
  MsQuicOpenVersion failed with; QUIC_STATUS_NOT_FOUND when the library or the
  export is missing, QUIC_STATUS_INTERNAL_ERROR when this binding's
  QUIC_SETTINGS does not have the size the library expects. Calling it twice
  is a no-op. }
function MsQuicLoad(const ALibrary: string = ''): QUIC_STATUS;
/// Closes the API table and unloads the library.
procedure MsQuicUnload;
/// Whether the library is loaded.
function MsQuicIsLoaded: Boolean;
/// Why the last MsQuicLoad failed, in a form worth showing the user.
function MsQuicLoadError: string;

/// Human readable form of a QUIC_STATUS, falling back to hex.
function QuicStatusToStr(const AStatus: QUIC_STATUS): string;
{ Points a QUIC_BUFFER at an ALPN string. The buffer borrows AAlpn's bytes,
  so AAlpn has to outlive every call that receives the buffer. }
procedure QuicSetAlpn(out ABuffer: QUIC_BUFFER; const AAlpn: AnsiString);
/// The loaded library's own version.
function MsQuicVersionStr: string;

implementation

type
  /// Signature of MsQuicOpenVersion.
  TMsQuicOpenVersion = function(Version: Cardinal;
    out QuicApi: Pointer): QUIC_STATUS; cdecl;
  /// Signature of MsQuicClose.
  TMsQuicClose = procedure(QuicApi: Pointer); cdecl;

var
  /// Handle of the loaded library, 0 when none.
  FLibHandle: {$IFDEF FPC}TLibHandle{$ELSE}HMODULE{$ENDIF} = 0;
  /// MsQuicClose of the loaded library.
  FClose: TMsQuicClose = nil;
  /// Why the last load failed.
  FLoadError: string = '';

{ What the loader itself has to say about the load that just failed. Worth the
  few lines because the ways it fails look identical from here and each has its
  own fix: on Android the library was never deployed, or it is the wrong ABI, or
  it needs a newer API level; on Windows a 32/64 bit mismatch reads the same as
  a missing file. Empty when the platform has nothing to add. }
function LibLoadError: string;
{$IF not Defined(FPC) and not Defined(MSWINDOWS)}
var
  vText: MarshaledAString;
{$IFEND}
begin
  {$IFDEF FPC}
  Result := GetLoadErrorStr;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result := SysErrorMessage(GetLastError);
  {$ELSE}
  vText := dlerror;
  if vText <> nil then
    Result := string(vText)
  else
    Result := '';
  {$ENDIF}
  {$ENDIF}
end;

function QUIC_FAILED(const AStatus: QUIC_STATUS): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (AStatus and $80000000) <> 0;
  {$ELSE}
  Result := Integer(AStatus) > 0;
  {$ENDIF}
end;

function QUIC_SUCCEEDED(const AStatus: QUIC_STATUS): Boolean;
begin
  Result := not QUIC_FAILED(AStatus);
end;

function QuicStatusIsCertError(const AStatus: QUIC_STATUS): Boolean;
begin
  Result := (AStatus = QUIC_STATUS_CERT_EXPIRED) or
            (AStatus = QUIC_STATUS_CERT_UNTRUSTED_ROOT) or
            (AStatus = QUIC_STATUS_CERT_NO_CERT) or
            ((AStatus and $FFFFFF00) = QUIC_STATUS_TLS_ALERT_BASE)
            { Windows: the whole FACILITY_CERT range - CERT_E_* and TRUST_E_*,
              of which the three constants above are only part }
            {$IFDEF MSWINDOWS} or ((AStatus and $FFFF0000) = $800B0000){$ENDIF}
            {$IFNDEF MSWINDOWS} or ((AStatus and $FFFFFF00) = $0BEBC400){$ENDIF};
end;

function QuicAddrToStr(const AAddr: QUIC_ADDR; out APort: Word): string;
var
  vWords: array[0..7] of Word;
  vInt, vRun, vRunLen, vBest, vBestLen: Integer;
  vMapped: Boolean;
begin
  Result := '';
  // same offset for both families, network order
  APort := (AAddr.v4_port shr 8) or ((AAddr.v4_port and $FF) shl 8);

  if AAddr.si_family = QUIC_ADDRESS_FAMILY_INET then
  begin
    // v4_addr holds the four bytes in network order
    Result := Format('%d.%d.%d.%d', [AAddr.v4_addr and $FF, (AAddr.v4_addr shr 8) and $FF,
      (AAddr.v4_addr shr 16) and $FF, (AAddr.v4_addr shr 24) and $FF]);
    Exit;
  end;
  if AAddr.si_family <> QUIC_ADDRESS_FAMILY_INET6 then
    Exit;

  // ::ffff:a.b.c.d - a v4 peer seen through a dual-stack socket
  vMapped := (AAddr.v6_addr[10] = $FF) and (AAddr.v6_addr[11] = $FF);
  for vInt := 0 to 9 do
    if AAddr.v6_addr[vInt] <> 0 then
      vMapped := False;
  if vMapped then
  begin
    Result := Format('%d.%d.%d.%d', [AAddr.v6_addr[12], AAddr.v6_addr[13],
      AAddr.v6_addr[14], AAddr.v6_addr[15]]);
    Exit;
  end;

  for vInt := 0 to 7 do
    vWords[vInt] := (Word(AAddr.v6_addr[vInt * 2]) shl 8) or AAddr.v6_addr[vInt * 2 + 1];

  // longest run of zero groups, two or more, is written as '::'
  vBest := -1;
  vBestLen := 0;
  vInt := 0;
  while vInt < 8 do
  begin
    if vWords[vInt] = 0 then
    begin
      vRun := vInt;
      vRunLen := 0;
      while (vInt < 8) and (vWords[vInt] = 0) do
      begin
        Inc(vRunLen);
        Inc(vInt);
      end;
      if (vRunLen >= 2) and (vRunLen > vBestLen) then
      begin
        vBest := vRun;
        vBestLen := vRunLen;
      end;
    end
    else
      Inc(vInt);
  end;

  vInt := 0;
  while vInt < 8 do
  begin
    if vInt = vBest then
    begin
      Result := Result + '::';
      Inc(vInt, vBestLen);
      Continue;
    end;
    if (Result <> '') and (Result[Length(Result)] <> ':') then
      Result := Result + ':';
    Result := Result + LowerCase(IntToHex(vWords[vInt], 1));
    Inc(vInt);
  end;
end;

function MsQuicIsLoaded: Boolean;
begin
  Result := MsQuicApi <> nil;
end;

function MsQuicLoadError: string;
begin
  Result := FLoadError;
end;

function QuicStatusToStr(const AStatus: QUIC_STATUS): string;
begin
  case AStatus of
    QUIC_STATUS_SUCCESS:              Result := 'SUCCESS';
    QUIC_STATUS_PENDING:              Result := 'PENDING';
    QUIC_STATUS_CONTINUE:             Result := 'CONTINUE';
    QUIC_STATUS_OUT_OF_MEMORY:        Result := 'OUT_OF_MEMORY';
    QUIC_STATUS_INVALID_PARAMETER:    Result := 'INVALID_PARAMETER';
    QUIC_STATUS_INVALID_STATE:        Result := 'INVALID_STATE';
    QUIC_STATUS_NOT_SUPPORTED:        Result := 'NOT_SUPPORTED';
    QUIC_STATUS_NOT_FOUND:            Result := 'NOT_FOUND';
    {$IFDEF MSWINDOWS} // the same errno as NOT_FOUND on POSIX
    QUIC_STATUS_FILE_NOT_FOUND:       Result := 'FILE_NOT_FOUND';
    {$ENDIF}
    QUIC_STATUS_BUFFER_TOO_SMALL:     Result := 'BUFFER_TOO_SMALL';
    QUIC_STATUS_HANDSHAKE_FAILURE:    Result := 'HANDSHAKE_FAILURE';
    QUIC_STATUS_ABORTED:              Result := 'ABORTED';
    QUIC_STATUS_ADDRESS_IN_USE:       Result := 'ADDRESS_IN_USE';
    QUIC_STATUS_INVALID_ADDRESS:      Result := 'INVALID_ADDRESS';
    QUIC_STATUS_CONNECTION_TIMEOUT:   Result := 'CONNECTION_TIMEOUT';
    QUIC_STATUS_CONNECTION_IDLE:      Result := 'CONNECTION_IDLE';
    QUIC_STATUS_UNREACHABLE:          Result := 'UNREACHABLE';
    QUIC_STATUS_INTERNAL_ERROR:       Result := 'INTERNAL_ERROR';
    QUIC_STATUS_CONNECTION_REFUSED:   Result := 'CONNECTION_REFUSED';
    QUIC_STATUS_PROTOCOL_ERROR:       Result := 'PROTOCOL_ERROR';
    QUIC_STATUS_VER_NEG_ERROR:        Result := 'VER_NEG_ERROR';
    QUIC_STATUS_TLS_ERROR:            Result := 'TLS_ERROR';
    QUIC_STATUS_USER_CANCELED:        Result := 'USER_CANCELED';
    QUIC_STATUS_ALPN_NEG_FAILURE:     Result := 'ALPN_NEG_FAILURE';
    QUIC_STATUS_STREAM_LIMIT_REACHED: Result := 'STREAM_LIMIT_REACHED';
    QUIC_STATUS_ALPN_IN_USE:          Result := 'ALPN_IN_USE';
    QUIC_STATUS_CERT_EXPIRED:         Result := 'CERT_EXPIRED';
    QUIC_STATUS_CERT_UNTRUSTED_ROOT:  Result := 'CERT_UNTRUSTED_ROOT';
    QUIC_STATUS_CERT_NO_CERT:         Result := 'CERT_NO_CERT';
    QUIC_STATUS_BAD_CERTIFICATE:      Result := 'BAD_CERTIFICATE';
  else
    if (AStatus and $FFFFFF00) = QUIC_STATUS_TLS_ALERT_BASE then
      Result := 'TLS_ALERT_' + IntToStr(AStatus and $FF)
    else
      Result := '0x' + IntToHex(AStatus, 8);
  end;
end;

procedure QuicSetAlpn(out ABuffer: QUIC_BUFFER; const AAlpn: AnsiString);
begin
  ABuffer.Length := System.Length(AAlpn);
  ABuffer.Buffer := PByte(PAnsiChar(AAlpn));
end;

function MsQuicVersionStr: string;
var
  vVer: array[0..3] of Cardinal;
  vLen: Cardinal;
begin
  Result := '';
  if MsQuicApi = nil then
    Exit;
  vLen := SizeOf(vVer);
  if QUIC_SUCCEEDED(MsQuicApi^.GetParam(nil, QUIC_PARAM_GLOBAL_LIBRARY_VERSION,
    @vLen, @vVer[0])) then
    Result := Format('%d.%d.%d.%d', [vVer[0], vVer[1], vVer[2], vVer[3]]);
end;

function MsQuicLoad(const ALibrary: string = ''): QUIC_STATUS;
var
  vName: string;
  vDetail: string;
  vOpen: TMsQuicOpenVersion;
  vApi: Pointer;
begin
  FLoadError := '';
  if MsQuicApi <> nil then
  begin
    Result := QUIC_STATUS_SUCCESS;
    Exit;
  end;

  {$IF DEFINED(DARWIN) OR DEFINED(MACOS) OR DEFINED(IOS)}
  // errno values are not the Linux ones here and the tables above would misread
  // every status - a failure as success included. Refusing is the honest answer.
  FLoadError := 'the MsQuic status codes of this platform are not translated';
  Result := QUIC_STATUS_NOT_SUPPORTED;
  Exit;
  {$IFEND}

  // A mismatch here would not fail loudly: MsQuic would read the settings it is
  // given at the offsets it expects and silently configure something else.
  if SizeOf(QUIC_SETTINGS) <> QUIC_SETTINGS_SIZE then
  begin
    FLoadError := Format('QUIC_SETTINGS is %d bytes, expected %d - the record' +
      ' layout does not match the one msquic.dll was built with',
      [SizeOf(QUIC_SETTINGS), QUIC_SETTINGS_SIZE]);
    Result := QUIC_STATUS_INTERNAL_ERROR;
    Exit;
  end;

  if ALibrary <> '' then
    vName := ALibrary
  else
    vName := MSQUIC_LIBRARY;

  {$IFDEF FPC}
  FLibHandle := LoadLibrary(vName);
  {$ELSE}
  FLibHandle := SafeLoadLibrary(vName);
  {$ENDIF}
  if FLibHandle = 0 then
  begin
    vDetail := LibLoadError;
    if vDetail <> '' then
      FLoadError := Format('could not load "%s": %s', [vName, vDetail])
    else
      FLoadError := Format('could not load "%s"', [vName]);
    Result := QUIC_STATUS_NOT_FOUND;
    Exit;
  end;

  vOpen := TMsQuicOpenVersion(GetProcAddress(FLibHandle, 'MsQuicOpenVersion'));
  FClose := TMsQuicClose(GetProcAddress(FLibHandle, 'MsQuicClose'));
  if (not Assigned(vOpen)) or (not Assigned(FClose)) then
  begin
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
    FClose := nil;
    FLoadError := Format('"%s" does not export MsQuicOpenVersion/MsQuicClose', [vName]);
    Result := QUIC_STATUS_NOT_FOUND;
    Exit;
  end;

  vApi := nil;
  Result := vOpen(QUIC_API_VERSION_2, vApi);
  if QUIC_FAILED(Result) or (vApi = nil) then
  begin
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
    FClose := nil;
    FLoadError := Format('MsQuicOpenVersion failed: %s', [QuicStatusToStr(Result)]);
    if QUIC_SUCCEEDED(Result) then
      Result := QUIC_STATUS_INTERNAL_ERROR;
    Exit;
  end;

  MsQuicApi := PQUIC_API_TABLE(vApi);
  Result := QUIC_STATUS_SUCCESS;
end;

procedure MsQuicUnload;
begin
  if MsQuicApi <> nil then
  begin
    if Assigned(FClose) then
      FClose(MsQuicApi);
    MsQuicApi := nil;
  end;
  FClose := nil;
  if FLibHandle <> 0 then
  begin
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
  end;
end;

end.
