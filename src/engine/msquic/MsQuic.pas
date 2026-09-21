/// Object Pascal binding for the MsQuic library (Microsoft QUIC), API version 2.
///
/// Translated against src/inc/msquic.h of the MsQuic 2.6.1 release
/// (github.com/microsoft/msquic). Status codes are per platform - HRESULTs on
/// Windows, errno values on Linux, as msquic_winuser.h and msquic_posix.h
/// define them - and so is QUIC_FAILED: on Windows failure is the high bit, on
/// POSIX it is any positive value, with PENDING and CONTINUE negative. Apple's
/// errno numbering differs from Linux and is not translated, so MsQuicLoad
/// refuses there instead of misreading every status.
///
/// Three things decide whether this binding is correct, and all three are
/// deliberate:
///
/// 1. Calling convention is cdecl. msquic.h defines QUIC_API as __cdecl. It is
///    irrelevant on x64, where there is only one convention, and it is not on
///    Win32.
/// 2. Record layout follows the MSVC ABI, which is what msquic.dll was built
///    with. Records that contain a 64-bit field are declared packed with
///    explicit padding, because MSVC aligns a 64-bit field on 8 bytes even on
///    32-bit targets while the i386 System V ABI that FPC follows aligns it on
///    4 - a plain record would disagree between compilers. Records without a
///    64-bit field are left unpacked, where Delphi and FPC both match MSVC.
/// 3. The event unions are raw bytes plus typed accessors. A Pascal variant
///    record would leave the offset of the variant part to the compiler; here
///    every variant that matters is its own small record whose layout can be
///    read off the C header line by line.
///
/// The API itself arrives as a table of function pointers from
/// MsQuicOpenVersion, so there is exactly one imported symbol and everything
/// else is a field of QUIC_API_TABLE.
unit MsQuic;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}
{$ENDIF}

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
  MSQUIC_LIBRARY = 'msquic.dll';
{$ELSE}
{$IFDEF ANDROID}
  // Android has no versioned sonames: the packager only carries lib/<abi>/*.so
  // into the APK, so a file called libmsquic.so.2 never reaches the device and
  // the plain name is what dlopen resolves, in the application's own library
  // folder. Both compilers define ANDROID for that target.
  MSQUIC_LIBRARY = 'libmsquic.so';
{$ELSE}
  MSQUIC_LIBRARY = 'libmsquic.so.2';
{$ENDIF}
{$ENDIF}

  QUIC_API_VERSION_2 = 2;

  QUIC_MAX_ALPN_LENGTH = 255;
  QUIC_MAX_SNI_LENGTH  = 65535;

type
  HQUIC  = Pointer;
  PHQUIC = ^HQUIC;

  /// HRESULT on Windows. Failure is the high bit, which is why QUIC_FAILED
  /// tests the bit instead of comparing with zero - on POSIX MsQuic uses errno
  /// values and the test is a different one.
  QUIC_STATUS = Cardinal;
  PQUIC_STATUS = ^QUIC_STATUS;

  QUIC_UINT62 = UInt64;
  /// ADDRESS_FAMILY on Windows, i.e. USHORT.
  QUIC_ADDRESS_FAMILY = Word;

const
{$IFDEF MSWINDOWS}
  // msquic_winuser.h: HRESULTs, failure is the high bit
  QUIC_STATUS_SUCCESS               = QUIC_STATUS($00000000);
  QUIC_STATUS_PENDING               = QUIC_STATUS($000703E5);
  QUIC_STATUS_CONTINUE              = QUIC_STATUS($000704DE);
  QUIC_STATUS_OUT_OF_MEMORY         = QUIC_STATUS($8007000E);
  QUIC_STATUS_INVALID_PARAMETER     = QUIC_STATUS($80070057);
  QUIC_STATUS_INVALID_STATE         = QUIC_STATUS($8007139F);
  QUIC_STATUS_NOT_SUPPORTED         = QUIC_STATUS($80004002);
  QUIC_STATUS_NOT_FOUND             = QUIC_STATUS($80070490);
  QUIC_STATUS_FILE_NOT_FOUND        = QUIC_STATUS($80070002);
  QUIC_STATUS_BUFFER_TOO_SMALL      = QUIC_STATUS($8007007A);
  QUIC_STATUS_HANDSHAKE_FAILURE     = QUIC_STATUS($80410000);
  QUIC_STATUS_ABORTED               = QUIC_STATUS($80004004);
  QUIC_STATUS_ADDRESS_IN_USE        = QUIC_STATUS($80072740);
  QUIC_STATUS_INVALID_ADDRESS       = QUIC_STATUS($80072741);
  QUIC_STATUS_CONNECTION_TIMEOUT    = QUIC_STATUS($80410006);
  QUIC_STATUS_CONNECTION_IDLE       = QUIC_STATUS($80410005);
  QUIC_STATUS_UNREACHABLE           = QUIC_STATUS($800704D0);
  QUIC_STATUS_INTERNAL_ERROR        = QUIC_STATUS($80410003);
  QUIC_STATUS_CONNECTION_REFUSED    = QUIC_STATUS($800704C9);
  QUIC_STATUS_PROTOCOL_ERROR        = QUIC_STATUS($80410004);
  QUIC_STATUS_VER_NEG_ERROR         = QUIC_STATUS($80410001);
  QUIC_STATUS_TLS_ERROR             = QUIC_STATUS($80072B18);
  QUIC_STATUS_USER_CANCELED         = QUIC_STATUS($80410002);
  QUIC_STATUS_ALPN_NEG_FAILURE      = QUIC_STATUS($80410007);
  QUIC_STATUS_STREAM_LIMIT_REACHED  = QUIC_STATUS($80410008);
  QUIC_STATUS_ALPN_IN_USE           = QUIC_STATUS($80410009);
  QUIC_STATUS_CERT_EXPIRED          = QUIC_STATUS($800B0101);
  QUIC_STATUS_CERT_UNTRUSTED_ROOT   = QUIC_STATUS($800B0109);
  QUIC_STATUS_CERT_NO_CERT          = QUIC_STATUS($8009030E);
  /// QUIC_STATUS_TLS_ALERT(42): what a callback returns to refuse a peer's
  /// certificate. The alerts occupy $80410100..$804101FF.
  QUIC_STATUS_BAD_CERTIFICATE       = QUIC_STATUS($8041012A);
  QUIC_STATUS_TLS_ALERT_BASE        = QUIC_STATUS($80410100);
{$ELSE}
  // msquic_posix.h on Linux: errno values, failure is any positive value.
  // Apple numbers most of these differently; MsQuicLoad refuses there.
  QUIC_STATUS_SUCCESS               = QUIC_STATUS(0);
  QUIC_STATUS_PENDING               = QUIC_STATUS($FFFFFFFE); // -2
  QUIC_STATUS_CONTINUE              = QUIC_STATUS($FFFFFFFF); // -1
  QUIC_STATUS_OUT_OF_MEMORY         = QUIC_STATUS(12);       // ENOMEM
  QUIC_STATUS_INVALID_PARAMETER     = QUIC_STATUS(22);       // EINVAL
  QUIC_STATUS_INVALID_STATE         = QUIC_STATUS(1);        // EPERM
  QUIC_STATUS_NOT_SUPPORTED         = QUIC_STATUS(95);       // EOPNOTSUPP
  QUIC_STATUS_NOT_FOUND             = QUIC_STATUS(2);        // ENOENT
  QUIC_STATUS_FILE_NOT_FOUND        = QUIC_STATUS(2);        // ENOENT
  QUIC_STATUS_BUFFER_TOO_SMALL      = QUIC_STATUS(75);       // EOVERFLOW
  QUIC_STATUS_HANDSHAKE_FAILURE     = QUIC_STATUS(103);      // ECONNABORTED
  QUIC_STATUS_ABORTED               = QUIC_STATUS(125);      // ECANCELED
  QUIC_STATUS_ADDRESS_IN_USE        = QUIC_STATUS(98);       // EADDRINUSE
  QUIC_STATUS_INVALID_ADDRESS       = QUIC_STATUS(97);       // EAFNOSUPPORT
  QUIC_STATUS_CONNECTION_TIMEOUT    = QUIC_STATUS(110);      // ETIMEDOUT
  QUIC_STATUS_CONNECTION_IDLE       = QUIC_STATUS(62);       // ETIME
  QUIC_STATUS_UNREACHABLE           = QUIC_STATUS(113);      // EHOSTUNREACH
  QUIC_STATUS_INTERNAL_ERROR        = QUIC_STATUS(5);        // EIO
  QUIC_STATUS_CONNECTION_REFUSED    = QUIC_STATUS(111);      // ECONNREFUSED
  QUIC_STATUS_PROTOCOL_ERROR        = QUIC_STATUS(71);       // EPROTO
  QUIC_STATUS_VER_NEG_ERROR         = QUIC_STATUS(93);       // EPROTONOSUPPORT
  QUIC_STATUS_TLS_ERROR             = QUIC_STATUS(126);      // ENOKEY
  QUIC_STATUS_USER_CANCELED         = QUIC_STATUS(130);      // EOWNERDEAD
  QUIC_STATUS_ALPN_NEG_FAILURE      = QUIC_STATUS(92);       // ENOPROTOOPT
  QUIC_STATUS_STREAM_LIMIT_REACHED  = QUIC_STATUS(86);       // ESTRPIPE
  QUIC_STATUS_ALPN_IN_USE           = QUIC_STATUS(91);       // EPROTOTYPE
  // QUIC_STATUS_CERT_ERROR(n) = $BEBC200 + n, QUIC_STATUS_TLS_ALERT(n) = $BEBC300 + n
  QUIC_STATUS_CERT_EXPIRED          = QUIC_STATUS($0BEBC201);
  QUIC_STATUS_CERT_UNTRUSTED_ROOT   = QUIC_STATUS($0BEBC202);
  QUIC_STATUS_CERT_NO_CERT          = QUIC_STATUS($0BEBC203);
  QUIC_STATUS_BAD_CERTIFICATE       = QUIC_STATUS($0BEBC32A);
  QUIC_STATUS_TLS_ALERT_BASE        = QUIC_STATUS($0BEBC300);
{$ENDIF}

  // QUIC_EXECUTION_PROFILE
  QUIC_EXECUTION_PROFILE_LOW_LATENCY         = 0;
  QUIC_EXECUTION_PROFILE_TYPE_MAX_THROUGHPUT = 1;
  QUIC_EXECUTION_PROFILE_TYPE_SCAVENGER      = 2;
  QUIC_EXECUTION_PROFILE_TYPE_REAL_TIME      = 3;

  // QUIC_CREDENTIAL_TYPE
  QUIC_CREDENTIAL_TYPE_NONE                       = 0;
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_HASH           = 1;
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_HASH_STORE     = 2;
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_CONTEXT        = 3;
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE           = 4;
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE_PROTECTED = 5;
  QUIC_CREDENTIAL_TYPE_CERTIFICATE_PKCS12         = 6;

  // QUIC_CREDENTIAL_FLAGS
  QUIC_CREDENTIAL_FLAG_NONE                          = $00000000;
  QUIC_CREDENTIAL_FLAG_CLIENT                        = $00000001;
  QUIC_CREDENTIAL_FLAG_LOAD_ASYNCHRONOUS             = $00000002;
  QUIC_CREDENTIAL_FLAG_NO_CERTIFICATE_VALIDATION     = $00000004;
  QUIC_CREDENTIAL_FLAG_INDICATE_CERTIFICATE_RECEIVED = $00000010;
  QUIC_CREDENTIAL_FLAG_DEFER_CERTIFICATE_VALIDATION  = $00000020;
  QUIC_CREDENTIAL_FLAG_REQUIRE_CLIENT_AUTHENTICATION = $00000040;
  QUIC_CREDENTIAL_FLAG_USE_TLS_BUILTIN_CERTIFICATE_VALIDATION = $00000080;
  QUIC_CREDENTIAL_FLAG_SET_ALLOWED_CIPHER_SUITES     = $00002000;
  QUIC_CREDENTIAL_FLAG_USE_PORTABLE_CERTIFICATES     = $00004000;
  QUIC_CREDENTIAL_FLAG_SET_CA_CERTIFICATE_FILE       = $00100000;

  // QUIC_SERVER_RESUMPTION_LEVEL
  QUIC_SERVER_NO_RESUME          = 0;
  QUIC_SERVER_RESUME_ONLY        = 1;
  QUIC_SERVER_RESUME_AND_ZERORTT = 2;

  // QUIC_CONNECTION_SHUTDOWN_FLAGS
  QUIC_CONNECTION_SHUTDOWN_FLAG_NONE   = $0000;
  QUIC_CONNECTION_SHUTDOWN_FLAG_SILENT = $0001;

  // QUIC_STREAM_OPEN_FLAGS
  QUIC_STREAM_OPEN_FLAG_NONE           = $0000;
  QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL = $0001;
  QUIC_STREAM_OPEN_FLAG_0_RTT          = $0002;

  // QUIC_STREAM_START_FLAGS
  QUIC_STREAM_START_FLAG_NONE             = $0000;
  QUIC_STREAM_START_FLAG_IMMEDIATE        = $0001;
  QUIC_STREAM_START_FLAG_FAIL_BLOCKED     = $0002;
  QUIC_STREAM_START_FLAG_SHUTDOWN_ON_FAIL = $0004;

  // QUIC_STREAM_SHUTDOWN_FLAGS
  QUIC_STREAM_SHUTDOWN_FLAG_NONE          = $0000;
  QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL      = $0001;
  QUIC_STREAM_SHUTDOWN_FLAG_ABORT_SEND    = $0002;
  QUIC_STREAM_SHUTDOWN_FLAG_ABORT_RECEIVE = $0004;
  QUIC_STREAM_SHUTDOWN_FLAG_ABORT         = $0006;
  QUIC_STREAM_SHUTDOWN_FLAG_IMMEDIATE     = $0008;

  // QUIC_RECEIVE_FLAGS
  QUIC_RECEIVE_FLAG_NONE  = $0000;
  QUIC_RECEIVE_FLAG_0_RTT = $0001;
  QUIC_RECEIVE_FLAG_FIN   = $0002;

  // QUIC_SEND_FLAGS
  QUIC_SEND_FLAG_NONE        = $0000;
  QUIC_SEND_FLAG_ALLOW_0_RTT = $0001;
  QUIC_SEND_FLAG_START       = $0002;
  QUIC_SEND_FLAG_FIN         = $0004;
  QUIC_SEND_FLAG_DELAY_SEND  = $0010;

  // QUIC_LISTENER_EVENT_TYPE
  QUIC_LISTENER_EVENT_NEW_CONNECTION   = 0;
  QUIC_LISTENER_EVENT_STOP_COMPLETE    = 1;
  QUIC_LISTENER_EVENT_DOS_MODE_CHANGED = 2;

  // QUIC_CONNECTION_EVENT_TYPE
  QUIC_CONNECTION_EVENT_CONNECTED                       = 0;
  QUIC_CONNECTION_EVENT_SHUTDOWN_INITIATED_BY_TRANSPORT = 1;
  QUIC_CONNECTION_EVENT_SHUTDOWN_INITIATED_BY_PEER      = 2;
  QUIC_CONNECTION_EVENT_SHUTDOWN_COMPLETE               = 3;
  QUIC_CONNECTION_EVENT_LOCAL_ADDRESS_CHANGED           = 4;
  QUIC_CONNECTION_EVENT_PEER_ADDRESS_CHANGED            = 5;
  QUIC_CONNECTION_EVENT_PEER_STREAM_STARTED             = 6;
  QUIC_CONNECTION_EVENT_STREAMS_AVAILABLE               = 7;
  QUIC_CONNECTION_EVENT_PEER_NEEDS_STREAMS              = 8;
  QUIC_CONNECTION_EVENT_IDEAL_PROCESSOR_CHANGED         = 9;
  QUIC_CONNECTION_EVENT_DATAGRAM_STATE_CHANGED          = 10;
  QUIC_CONNECTION_EVENT_DATAGRAM_RECEIVED               = 11;
  QUIC_CONNECTION_EVENT_DATAGRAM_SEND_STATE_CHANGED     = 12;
  QUIC_CONNECTION_EVENT_RESUMED                         = 13;
  QUIC_CONNECTION_EVENT_RESUMPTION_TICKET_RECEIVED      = 14;
  QUIC_CONNECTION_EVENT_PEER_CERTIFICATE_RECEIVED       = 15;

  // QUIC_STREAM_EVENT_TYPE
  QUIC_STREAM_EVENT_START_COMPLETE         = 0;
  QUIC_STREAM_EVENT_RECEIVE                = 1;
  QUIC_STREAM_EVENT_SEND_COMPLETE          = 2;
  QUIC_STREAM_EVENT_PEER_SEND_SHUTDOWN     = 3;
  QUIC_STREAM_EVENT_PEER_SEND_ABORTED      = 4;
  QUIC_STREAM_EVENT_PEER_RECEIVE_ABORTED   = 5;
  QUIC_STREAM_EVENT_SEND_SHUTDOWN_COMPLETE = 6;
  QUIC_STREAM_EVENT_SHUTDOWN_COMPLETE      = 7;
  QUIC_STREAM_EVENT_IDEAL_SEND_BUFFER_SIZE = 8;
  QUIC_STREAM_EVENT_PEER_ACCEPTED          = 9;
  QUIC_STREAM_EVENT_CANCEL_ON_LOSS         = 10;

  // Address families: AF_INET6 is 23 on Windows and 10 on Linux.
  QUIC_ADDRESS_FAMILY_UNSPEC = 0;
  QUIC_ADDRESS_FAMILY_INET   = 2;
  QUIC_ADDRESS_FAMILY_INET6  = {$IFDEF MSWINDOWS}23{$ELSE}10{$ENDIF};

  // QUIC_PARAM_*
  QUIC_PARAM_GLOBAL_LIBRARY_VERSION  = $01000004;
  QUIC_PARAM_GLOBAL_LIBRARY_GIT_HASH = $01000008;
  QUIC_PARAM_GLOBAL_TLS_PROVIDER     = $0100000A;
  QUIC_PARAM_CONFIGURATION_SETTINGS  = $03000000;
  QUIC_PARAM_LISTENER_LOCAL_ADDRESS  = $04000000;
  QUIC_PARAM_CONN_QUIC_VERSION       = $05000000;
  QUIC_PARAM_CONN_LOCAL_ADDRESS      = $05000001;
  QUIC_PARAM_CONN_REMOTE_ADDRESS     = $05000002;
  QUIC_PARAM_CONN_SETTINGS           = $05000004;
  QUIC_PARAM_TLS_NEGOTIATED_ALPN     = $06000001;
  QUIC_PARAM_STREAM_ID               = $08000000;

  // Bit positions of QUIC_SETTINGS.IsSetFlags, in header order.
  QUIC_SETTING_MaxBytesPerKey             = UInt64(1) shl 0;
  QUIC_SETTING_HandshakeIdleTimeoutMs     = UInt64(1) shl 1;
  QUIC_SETTING_IdleTimeoutMs              = UInt64(1) shl 2;
  QUIC_SETTING_StreamRecvWindowDefault    = UInt64(1) shl 6;
  QUIC_SETTING_StreamRecvBufferDefault    = UInt64(1) shl 7;
  QUIC_SETTING_ConnFlowControlWindow      = UInt64(1) shl 8;
  QUIC_SETTING_InitialWindowPackets       = UInt64(1) shl 11;
  QUIC_SETTING_SendIdleTimeoutMs          = UInt64(1) shl 12;
  QUIC_SETTING_InitialRttMs               = UInt64(1) shl 13;
  QUIC_SETTING_MaxAckDelayMs              = UInt64(1) shl 14;
  QUIC_SETTING_DisconnectTimeoutMs        = UInt64(1) shl 15;
  QUIC_SETTING_KeepAliveIntervalMs        = UInt64(1) shl 16;
  QUIC_SETTING_CongestionControlAlgorithm = UInt64(1) shl 17;
  QUIC_SETTING_PeerBidiStreamCount        = UInt64(1) shl 18;
  QUIC_SETTING_PeerUnidiStreamCount       = UInt64(1) shl 19;
  QUIC_SETTING_MinimumMtu                 = UInt64(1) shl 22;
  QUIC_SETTING_MaximumMtu                 = UInt64(1) shl 23;
  QUIC_SETTING_SendBufferingEnabled       = UInt64(1) shl 24;
  QUIC_SETTING_PacingEnabled              = UInt64(1) shl 25;
  QUIC_SETTING_MigrationEnabled           = UInt64(1) shl 26;
  QUIC_SETTING_DatagramReceiveEnabled     = UInt64(1) shl 27;
  QUIC_SETTING_ServerResumptionLevel      = UInt64(1) shl 28;
  QUIC_SETTING_GreaseQuicBitEnabled       = UInt64(1) shl 32;
  QUIC_SETTING_EcnEnabled                 = UInt64(1) shl 33;

  // Bits of QUIC_SETTINGS.BitFlags, LSB first as MSVC packs them.
  QUIC_SETTINGS_BIT_SendBufferingEnabled   = $01;
  QUIC_SETTINGS_BIT_PacingEnabled          = $02;
  QUIC_SETTINGS_BIT_MigrationEnabled       = $04;
  QUIC_SETTINGS_BIT_DatagramReceiveEnabled = $08;
  QUIC_SETTINGS_SHIFT_ServerResumptionLevel = 4; // 2 bits wide
  QUIC_SETTINGS_BIT_GreaseQuicBitEnabled   = $40;
  QUIC_SETTINGS_BIT_EcnEnabled             = $80;

  /// sizeof(QUIC_SETTINGS) as MSVC lays it out. Checked at load time, because a
  /// silent mismatch here misconfigures the connection instead of failing.
  QUIC_SETTINGS_SIZE = 144;

type
  /// SOCKADDR_INET. Every field sits at its naturally aligned offset, so packed
  /// reproduces the C layout exactly: 28 bytes.
  QUIC_ADDR = packed record
    case Integer of
      0: (si_family: Word);
      1: (v4_family: Word; v4_port: Word; v4_addr: Cardinal;
          v4_zero: array[0..7] of Byte);
      2: (v6_family: Word; v6_port: Word; v6_flowinfo: Cardinal;
          v6_addr: array[0..15] of Byte; v6_scope_id: Cardinal);
  end;
  PQUIC_ADDR = ^QUIC_ADDR;

  /// uint32 followed by a pointer. No 64-bit field, so the natural layout of
  /// both Pascal compilers already matches MSVC.
  QUIC_BUFFER = record
    Length: Cardinal;
    Buffer: PByte;
  end;
  PQUIC_BUFFER = ^QUIC_BUFFER;
  TQuicBufferArray = array[0..1023] of QUIC_BUFFER;
  PQuicBufferArray = ^TQuicBufferArray;

  QUIC_REGISTRATION_CONFIG = record
    AppName: PAnsiChar;
    ExecutionProfile: Cardinal;
  end;
  PQUIC_REGISTRATION_CONFIG = ^QUIC_REGISTRATION_CONFIG;

  QUIC_CERTIFICATE_FILE = record
    PrivateKeyFile: PAnsiChar;
    CertificateFile: PAnsiChar;
  end;
  PQUIC_CERTIFICATE_FILE = ^QUIC_CERTIFICATE_FILE;

  /// QUIC_CREDENTIAL_TYPE_CERTIFICATE_FILE_PROTECTED: the same two files plus
  /// the password of an encrypted private key.
  QUIC_CERTIFICATE_FILE_PROTECTED = record
    PrivateKeyFile: PAnsiChar;
    CertificateFile: PAnsiChar;
    PrivateKeyPassword: PAnsiChar;
  end;
  PQUIC_CERTIFICATE_FILE_PROTECTED = ^QUIC_CERTIFICATE_FILE_PROTECTED;

  QUIC_CERTIFICATE_PKCS12 = record
    Asn1Blob: PByte;
    Asn1BlobLength: Cardinal;
    PrivateKeyPassword: PAnsiChar;
  end;
  PQUIC_CERTIFICATE_PKCS12 = ^QUIC_CERTIFICATE_PKCS12;

  QUIC_CERTIFICATE_HASH = record
    ShaHash: array[0..19] of Byte;
  end;
  PQUIC_CERTIFICATE_HASH = ^QUIC_CERTIFICATE_HASH;

  /// The C union is a single pointer, so it is declared as one; the caller
  /// casts whichever certificate record matches CredType into CertificateRef.
  QUIC_CREDENTIAL_CONFIG = record
    CredType: Cardinal;
    Flags: Cardinal;
    CertificateRef: Pointer;
    Principal: PAnsiChar;
    Reserved: Pointer;
    AsyncHandler: Pointer;
    AllowedCipherSuites: Cardinal;
    CaCertificateFile: PAnsiChar;
  end;
  PQUIC_CREDENTIAL_CONFIG = ^QUIC_CREDENTIAL_CONFIG;

  QUIC_NEW_CONNECTION_INFO = record
    QuicVersion: Cardinal;
    LocalAddress: PQUIC_ADDR;
    RemoteAddress: PQUIC_ADDR;
    CryptoBufferLength: Cardinal;
    ClientAlpnListLength: Word;
    ServerNameLength: Word;
    NegotiatedAlpnLength: Byte;
    CryptoBuffer: PByte;
    ClientAlpnList: PByte;
    NegotiatedAlpn: PByte;
    ServerName: PAnsiChar;
  end;
  PQUIC_NEW_CONNECTION_INFO = ^QUIC_NEW_CONNECTION_INFO;

  /// Packed with explicit padding: it mixes 64-, 32-, 16- and 8-bit fields, and
  /// MSVC's 8-byte alignment of the 64-bit ones is what has to be reproduced.
  /// The offsets in the comments are what msquic.dll expects.
  QUIC_SETTINGS = packed record
    IsSetFlags: UInt64;                          //   0
    MaxBytesPerKey: UInt64;                      //   8
    HandshakeIdleTimeoutMs: UInt64;              //  16
    IdleTimeoutMs: UInt64;                       //  24
    MtuDiscoverySearchCompleteTimeoutUs: UInt64; //  32
    TlsClientMaxSendBuffer: Cardinal;            //  40
    TlsServerMaxSendBuffer: Cardinal;            //  44
    StreamRecvWindowDefault: Cardinal;           //  48
    StreamRecvBufferDefault: Cardinal;           //  52
    ConnFlowControlWindow: Cardinal;             //  56
    MaxWorkerQueueDelayUs: Cardinal;             //  60
    MaxStatelessOperations: Cardinal;            //  64
    InitialWindowPackets: Cardinal;              //  68
    SendIdleTimeoutMs: Cardinal;                 //  72
    InitialRttMs: Cardinal;                      //  76
    MaxAckDelayMs: Cardinal;                     //  80
    DisconnectTimeoutMs: Cardinal;               //  84
    KeepAliveIntervalMs: Cardinal;               //  88
    CongestionControlAlgorithm: Word;            //  92
    PeerBidiStreamCount: Word;                   //  94
    PeerUnidiStreamCount: Word;                  //  96
    MaxBindingStatelessOperations: Word;         //  98
    StatelessOperationExpirationMs: Word;        // 100
    MinimumMtu: Word;                            // 102
    MaximumMtu: Word;                            // 104
    BitFlags: Byte;                              // 106
    MaxOperationsPerDrain: Byte;                 // 107
    MtuDiscoveryMissingProbeCount: Byte;         // 108
    Pad0: array[0..2] of Byte;                   // 109 - up to the uint32
    DestCidUpdateIdleTimeoutMs: Cardinal;        // 112
    Pad1: Cardinal;                              // 116 - up to the uint64
    Flags: UInt64;                               // 120
    StreamRecvWindowBidiLocalDefault: Cardinal;  // 128
    StreamRecvWindowBidiRemoteDefault: Cardinal; // 132
    StreamRecvWindowUnidiDefault: Cardinal;      // 136
    Pad2: Cardinal;                              // 140 - tail padding
  end;                                           // 144
  PQUIC_SETTINGS = ^QUIC_SETTINGS;

  //
  // Events. Every event is a 32-bit type followed by a union, and where that
  // union begins is decided by the widest alignment among its members. The
  // union itself is raw bytes here, read through the typed records below.
  //
  // The connection and stream unions both contain members with a 64-bit field
  // (QUIC_UINT62 error codes, the receive offsets), so their alignment is 8 and
  // the union starts at offset 8 on 32- and 64-bit alike - MSVC aligns a 64-bit
  // field on 8 bytes on x86 too.
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

  QUIC_LISTENER_EVENT = packed record
    EventType: Cardinal;
    {$IFDEF QUIC_PTR64}Pad: Cardinal;{$ENDIF}
    Data: array[0..63] of Byte;
  end;
  PQUIC_LISTENER_EVENT = ^QUIC_LISTENER_EVENT;

  QUIC_CONNECTION_EVENT = packed record
    EventType: Cardinal;
    Pad: Cardinal;
    Data: array[0..95] of Byte;
  end;
  PQUIC_CONNECTION_EVENT = ^QUIC_CONNECTION_EVENT;

  QUIC_STREAM_EVENT = packed record
    EventType: Cardinal;
    Pad: Cardinal;
    Data: array[0..63] of Byte;
  end;
  PQUIC_STREAM_EVENT = ^QUIC_STREAM_EVENT;

  /// QUIC_LISTENER_EVENT.NEW_CONNECTION
  TQuicNewConnectionData = packed record
    Info: PQUIC_NEW_CONNECTION_INFO;
    Connection: HQUIC;
  end;
  PQuicNewConnectionData = ^TQuicNewConnectionData;

  /// QUIC_CONNECTION_EVENT.CONNECTED
  TQuicConnectedData = packed record
    SessionResumed: ByteBool;
    NegotiatedAlpnLength: Byte;
    Pad: array[0..SizeOf(Pointer) - 3] of Byte;
    NegotiatedAlpn: PByte;
  end;
  PQuicConnectedData = ^TQuicConnectedData;

  /// QUIC_CONNECTION_EVENT.SHUTDOWN_INITIATED_BY_TRANSPORT
  TQuicShutdownByTransportData = packed record
    Status: QUIC_STATUS;
    Pad: Cardinal;
    ErrorCode: QUIC_UINT62;
  end;
  PQuicShutdownByTransportData = ^TQuicShutdownByTransportData;

  /// QUIC_CONNECTION_EVENT.SHUTDOWN_INITIATED_BY_PEER
  TQuicShutdownByPeerData = packed record
    ErrorCode: QUIC_UINT62;
  end;
  PQuicShutdownByPeerData = ^TQuicShutdownByPeerData;

  /// QUIC_CONNECTION_EVENT.PEER_STREAM_STARTED
  TQuicPeerStreamStartedData = packed record
    Stream: HQUIC;
    Flags: Cardinal;
  end;
  PQuicPeerStreamStartedData = ^TQuicPeerStreamStartedData;

  /// QUIC_CONNECTION_EVENT.PEER_CERTIFICATE_RECEIVED, indicated only with
  /// QUIC_CREDENTIAL_FLAG_INDICATE_CERTIFICATE_RECEIVED. With
  /// QUIC_CREDENTIAL_FLAG_USE_PORTABLE_CERTIFICATES the two pointers are
  /// QUIC_BUFFERs: the DER of the peer's certificate, and the PKCS#7 DER of
  /// its chain. With DEFER_CERTIFICATE_VALIDATION the library's own verdict
  /// arrives in DeferredStatus (SUCCESS when it trusted the certificate) and
  /// the callback's return value is the final decision.
  TQuicPeerCertificateReceivedData = packed record
    Certificate: Pointer;
    DeferredErrorFlags: Cardinal;
    DeferredStatus: QUIC_STATUS;
    Chain: Pointer;
  end;
  PQuicPeerCertificateReceivedData = ^TQuicPeerCertificateReceivedData;

  /// QUIC_STREAM_EVENT.RECEIVE
  TQuicReceiveData = packed record
    AbsoluteOffset: UInt64;
    TotalBufferLength: UInt64;
    Buffers: PQuicBufferArray;
    BufferCount: Cardinal;
    Flags: Cardinal;
  end;
  PQuicReceiveData = ^TQuicReceiveData;

  /// QUIC_STREAM_EVENT.SEND_COMPLETE
  TQuicSendCompleteData = packed record
    Canceled: ByteBool;
    Pad: array[0..SizeOf(Pointer) - 2] of Byte;
    ClientContext: Pointer;
  end;
  PQuicSendCompleteData = ^TQuicSendCompleteData;

  /// QUIC_STREAM_EVENT.START_COMPLETE
  TQuicStartCompleteData = packed record
    Status: QUIC_STATUS;
    Pad: Cardinal;
    ID: QUIC_UINT62;
    PeerAccepted: Byte;
  end;
  PQuicStartCompleteData = ^TQuicStartCompleteData;

  QUIC_LISTENER_CALLBACK = function(Listener: HQUIC; Context: Pointer;
    Event: PQUIC_LISTENER_EVENT): QUIC_STATUS; cdecl;
  QUIC_CONNECTION_CALLBACK = function(Connection: HQUIC; Context: Pointer;
    Event: PQUIC_CONNECTION_EVENT): QUIC_STATUS; cdecl;
  QUIC_STREAM_CALLBACK = function(Stream: HQUIC; Context: Pointer;
    Event: PQUIC_STREAM_EVENT): QUIC_STATUS; cdecl;

  /// The API function table returned by MsQuicOpenVersion. Declared up to
  /// ConnectionOpenInPartition (MsQuic 2.5); the preview-only entries that
  /// follow are simply not described here, which is safe because the record
  /// only ever describes a prefix of a table the library owns.
  QUIC_API_TABLE = record
    SetContext: procedure(Handle: HQUIC; Context: Pointer); cdecl;
    GetContext: function(Handle: HQUIC): Pointer; cdecl;
    SetCallbackHandler: procedure(Handle: HQUIC; Handler: Pointer;
      Context: Pointer); cdecl;

    SetParam: function(Handle: HQUIC; Param: Cardinal; BufferLength: Cardinal;
      Buffer: Pointer): QUIC_STATUS; cdecl;
    GetParam: function(Handle: HQUIC; Param: Cardinal; BufferLength: PCardinal;
      Buffer: Pointer): QUIC_STATUS; cdecl;

    RegistrationOpen: function(Config: PQUIC_REGISTRATION_CONFIG;
      out Registration: HQUIC): QUIC_STATUS; cdecl;
    RegistrationClose: procedure(Registration: HQUIC); cdecl;
    RegistrationShutdown: procedure(Registration: HQUIC; Flags: Cardinal;
      ErrorCode: QUIC_UINT62); cdecl;

    ConfigurationOpen: function(Registration: HQUIC;
      const AlpnBuffers: PQUIC_BUFFER; AlpnBufferCount: Cardinal;
      Settings: PQUIC_SETTINGS; SettingsSize: Cardinal; Context: Pointer;
      out Configuration: HQUIC): QUIC_STATUS; cdecl;
    ConfigurationClose: procedure(Configuration: HQUIC); cdecl;
    ConfigurationLoadCredential: function(Configuration: HQUIC;
      CredConfig: PQUIC_CREDENTIAL_CONFIG): QUIC_STATUS; cdecl;

    ListenerOpen: function(Registration: HQUIC; Handler: QUIC_LISTENER_CALLBACK;
      Context: Pointer; out Listener: HQUIC): QUIC_STATUS; cdecl;
    ListenerClose: procedure(Listener: HQUIC); cdecl;
    ListenerStart: function(Listener: HQUIC; const AlpnBuffers: PQUIC_BUFFER;
      AlpnBufferCount: Cardinal; LocalAddress: PQUIC_ADDR): QUIC_STATUS; cdecl;
    ListenerStop: procedure(Listener: HQUIC); cdecl;

    ConnectionOpen: function(Registration: HQUIC;
      Handler: QUIC_CONNECTION_CALLBACK; Context: Pointer;
      out Connection: HQUIC): QUIC_STATUS; cdecl;
    ConnectionClose: procedure(Connection: HQUIC); cdecl;
    ConnectionShutdown: procedure(Connection: HQUIC; Flags: Cardinal;
      ErrorCode: QUIC_UINT62); cdecl;
    ConnectionStart: function(Connection: HQUIC; Configuration: HQUIC;
      Family: QUIC_ADDRESS_FAMILY; ServerName: PAnsiChar;
      ServerPort: Word): QUIC_STATUS; cdecl;
    ConnectionSetConfiguration: function(Connection: HQUIC;
      Configuration: HQUIC): QUIC_STATUS; cdecl;
    ConnectionSendResumptionTicket: function(Connection: HQUIC; Flags: Cardinal;
      DataLength: Word; ResumptionData: PByte): QUIC_STATUS; cdecl;

    StreamOpen: function(Connection: HQUIC; Flags: Cardinal;
      Handler: QUIC_STREAM_CALLBACK; Context: Pointer;
      out Stream: HQUIC): QUIC_STATUS; cdecl;
    StreamClose: procedure(Stream: HQUIC); cdecl;
    StreamStart: function(Stream: HQUIC; Flags: Cardinal): QUIC_STATUS; cdecl;
    StreamShutdown: function(Stream: HQUIC; Flags: Cardinal;
      ErrorCode: QUIC_UINT62): QUIC_STATUS; cdecl;
    StreamSend: function(Stream: HQUIC; const Buffers: PQUIC_BUFFER;
      BufferCount: Cardinal; Flags: Cardinal;
      ClientSendContext: Pointer): QUIC_STATUS; cdecl;
    StreamReceiveComplete: procedure(Stream: HQUIC; BufferLength: UInt64); cdecl;
    StreamReceiveSetEnabled: function(Stream: HQUIC;
      IsEnabled: ByteBool): QUIC_STATUS; cdecl;

    DatagramSend: function(Connection: HQUIC; const Buffers: PQUIC_BUFFER;
      BufferCount: Cardinal; Flags: Cardinal;
      ClientSendContext: Pointer): QUIC_STATUS; cdecl;

    ConnectionResumptionTicketValidationComplete: function(Connection: HQUIC;
      AResult: ByteBool): QUIC_STATUS; cdecl;
    ConnectionCertificateValidationComplete: function(Connection: HQUIC;
      AResult: ByteBool; TlsAlert: Cardinal): QUIC_STATUS; cdecl;

    ConnectionOpenInPartition: function(Registration: HQUIC;
      PartitionIndex: Word; Handler: QUIC_CONNECTION_CALLBACK; Context: Pointer;
      out Connection: HQUIC): QUIC_STATUS; cdecl;
  end;
  PQUIC_API_TABLE = ^QUIC_API_TABLE;

var
  /// Valid between a successful MsQuicLoad and MsQuicUnload.
  MsQuicApi: PQUIC_API_TABLE = nil;

function QUIC_FAILED(const AStatus: QUIC_STATUS): Boolean;
function QUIC_SUCCEEDED(const AStatus: QUIC_STATUS): Boolean;
/// Whether a status is about the peer's certificate - a TLS alert or one of
/// the CERT_* codes - as opposed to the network or the library.
function QuicStatusIsCertError(const AStatus: QUIC_STATUS): Boolean;
/// Text of a QUIC_ADDR: dotted IPv4, or IPv6 with its longest zero run
/// collapsed the way RFC 5952 writes it. An IPv4-mapped IPv6 address comes
/// back as the IPv4 it carries, which is what a dual-stack listener reports
/// for a v4 peer. APort receives the port in host order.
function QuicAddrToStr(const AAddr: QUIC_ADDR; out APort: Word): string;

/// Loads msquic and fills MsQuicApi. Returns QUIC_STATUS_SUCCESS, or the status
/// MsQuicOpenVersion failed with; QUIC_STATUS_NOT_FOUND when the library or the
/// export is missing, QUIC_STATUS_INTERNAL_ERROR when this binding's
/// QUIC_SETTINGS does not have the size the library expects. Calling it twice
/// is a no-op.
function MsQuicLoad(const ALibrary: string = ''): QUIC_STATUS;
procedure MsQuicUnload;
function MsQuicIsLoaded: Boolean;
/// Why the last MsQuicLoad failed, in a form worth showing the user.
function MsQuicLoadError: string;

/// Human readable form of a QUIC_STATUS, falling back to hex.
function QuicStatusToStr(const AStatus: QUIC_STATUS): string;
/// Points a QUIC_BUFFER at an ALPN string. The buffer borrows AAlpn's bytes,
/// so AAlpn has to outlive every call that receives the buffer.
procedure QuicSetAlpn(out ABuffer: QUIC_BUFFER; const AAlpn: AnsiString);
/// The loaded library's own version.
function MsQuicVersionStr: string;

implementation

type
  TMsQuicOpenVersion = function(Version: Cardinal;
    out QuicApi: Pointer): QUIC_STATUS; cdecl;
  TMsQuicClose = procedure(QuicApi: Pointer); cdecl;

var
  FLibHandle: {$IFDEF FPC}TLibHandle{$ELSE}HMODULE{$ENDIF} = 0;
  FClose: TMsQuicClose = nil;
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
            {$IFNDEF MSWINDOWS} or ((AStatus and $FFFFFF00) = $0BEBC200){$ENDIF};
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
