/// TLS for the MQTT transports, over OpenSSL (1.1.1 or 3.x) loaded at run
/// time: the same code on every system. Client and server, SNI, ALPN, the
/// server name checked against the certificate, mutual TLS, and the rule of
/// TRALClient for which certificate is accepted: the event decides, else the
/// pin, else the verdict of OpenSSL, which covers chain and name.
unit RALMQTTTLS;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\..\core\PascalRAL.inc}

{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  {$IFDEF RALWindows}
  Windows,
  {$ENDIF}
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALClient, RALExternalsLibraries, RALMQTTTypes, RALMQTTSocket;

type
  /// Lowest TLS version accepted.
  TRALMQTTTLSVersion = (
    /// TLS 1.2 (RFC 5246) and above.
    mtv12,
    /// TLS 1.3 (RFC 8446) only.
    mtv13);

  { TLS options of an MQTT client. Verify, Pins and Required work as on
    TRALClient (the same class): with no pin and no event, svEngine and
    svAlways both take the verdict of OpenSSL (chain and host name), svNever
    accepts any certificate }
  TRALMQTTSSL = class(TRALClientSSL)
  private
    FALPN: StringRAL;
    FCAFile: StringRAL;
    FCertFile: StringRAL;
    FEnabled: boolean;
    FKeyFile: StringRAL;
    FKeyPassword: StringRAL;
    FMinVersion: TRALMQTTTLSVersion;
    FServerName: StringRAL;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    { ALPN protocols offered, separated by commas; empty offers none. Cloud
      brokers on port 443 want 'mqtt' }
    property ALPN: StringRAL read FALPN write FALPN;
    /// File of CA certificates (PEM) trusted besides the system's.
    property CAFile: StringRAL read FCAFile write FCAFile;
    /// Client certificate (PEM, chain allowed), for mutual TLS.
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Use TLS.
    property Enabled: boolean read FEnabled write FEnabled default False;
    /// Private key of CertFile (PEM).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Password of KeyFile, when it is encrypted.
    property KeyPassword: StringRAL read FKeyPassword write FKeyPassword;
    /// Lowest TLS version accepted.
    property MinVersion: TRALMQTTTLSVersion read FMinVersion write FMinVersion default mtv12;
    { Name sent in SNI and checked against the certificate; empty is the host
      connected to }
    property ServerName: StringRAL read FServerName write FServerName;
  end;

  /// What an SSL_CTX is made for.
  TRALMQTTTLSRole = (mtrClient, mtrServer);

  { An OpenSSL context: one per client connection, one per broker listener.
    Holds the certificate, key and trust settings; channels are made from it }
  TRALMQTTTLSContext = class
  private
    FALPN: StringRAL;
    FContext: Pointer;
    FDatagram: boolean;
    FKeyPassword: AnsiString;
    FRole: TRALMQTTTLSRole;
  public
    { A context for ARole; ADatagram makes it DTLS (MQTT-SN over UDP).
      Raises when OpenSSL cannot be loaded }
    constructor Create(ARole: TRALMQTTTLSRole; ADatagram: boolean = False);
    destructor Destroy; override;

    /// Trusts the CA certificates in AFile (PEM) besides the system's.
    procedure LoadCAFile(const AFile: StringRAL);
    /// Trusts the system's CA certificates (the Windows root store included).
    procedure LoadSystemCAs;
    /// Lowest version accepted.
    procedure SetMinVersion(AVersion: TRALMQTTTLSVersion);
    { Certificate (with its chain) and key, from files; APassword opens an
      encrypted key }
    procedure UseCertificateFiles(const ACertFile, AKeyFile, APassword: StringRAL);
    /// Certificate (with its chain) and key, from PEM text.
    procedure UseCertificatePEM(const ACertPEM, AKeyPEM: StringRAL);
    { Server: ask the client for a certificate (mutual TLS); ARequire refuses
      the handshake without one. ACAFile is the CA the client certificates
      must come from }
    procedure RequestClientCertificate(ARequire: boolean; const ACAFile: StringRAL);

    { ALPN: the client offers these protocols (comma separated); the server
      picks the first of its own it is offered, or none }
    property ALPN: StringRAL read FALPN write FALPN;
    /// The SSL_CTX.
    property Handle: Pointer read FContext;
    /// Client or server.
    property Role: TRALMQTTTLSRole read FRole;
  end;

  { TLS over a socket channel. The client handshake waits with a deadline
    (Handshake); the server one advances a step per call (HandshakeStep), for
    the broker's event loop. After it, Read and Write never wait }
  TRALMQTTTLSChannel = class(TRALMQTTLayerChannel)
  private
    FContext: TRALMQTTTLSContext;
    FDone: boolean;
    FSSL: Pointer;
    FWant: Word;

    function CheckIO(ARes: Integer; const AWhat: string): IntegerRAL;
  protected
    function PeerCertificateDER: TBytes;
  public
    { TLS over AInner (a socket channel, already connected), from AContext;
      the context is not owned }
    constructor Create(AInner: TRALMQTTChannel; AContext: TRALMQTTTLSContext;
      AOwnsInner: boolean = True);
    destructor Destroy; override;

    { Client handshake with AServerName in SNI and as the name the
      certificate must carry (an address literal is checked as such, without
      SNI), within ATimeout milliseconds }
    procedure Handshake(const AServerName: StringRAL; ATimeout: IntegerRAL);
    { One step of the server handshake: True when it finished; False when it
      needs the socket again (WantedEvents says for what). Raises when it failed }
    function HandshakeStep: boolean;
    { The server certificate as TRALClient describes it, with Trusted = the
      chain verified and the name matched, and Error saying why not }
    function PeerCertificate: TRALCertInfo;
    /// The protocol ALPN settled on, or empty.
    function SelectedALPN: StringRAL;
    /// The TLS version in use ('TLSv1.3').
    function VersionName: StringRAL;

    procedure Close; override;
    function HasBuffered: boolean; override;
    function Read(var ABuffer; ACount: IntegerRAL): IntegerRAL; override;
    function WantedEvents: Word; override;
    function Write(const ABuffer; ACount: IntegerRAL): IntegerRAL; override;

    /// The handshake is over.
    property Done: boolean read FDone;
  end;

  /// One application datagram that came out of a DTLS session.
  TRALMQTTDTLSPlain = class
  public
    /// The packet, decrypted.
    Data: TBytes;
  end;

  { DTLS (MQTT-SN over UDP) on memory: datagrams go in by Feed and come out by
    TakeOutput, so the caller keeps its socket and many sessions can share one
    (a gateway answers every client on a single UDP port). A flight of the
    handshake leaves as one datagram, its records joined, which DTLS allows }
  TRALMQTTDTLSSession = class
  private
    FContext: TRALMQTTTLSContext;
    FDone: boolean;
    FRead: Pointer;
    FServer: boolean;
    FSSL: Pointer;
    FWrite: Pointer;

    procedure Step;
  public
    { A session of AContext (made with ADatagram); AServer answers a client's
      handshake, otherwise the session starts one for AServerName }
    constructor Create(AContext: TRALMQTTTLSContext; AServer: boolean;
      const AServerName: StringRAL = '');
    destructor Destroy; override;

    { Takes a datagram from the peer: handshake records advance the handshake,
      application records come back in APlain (one MQTT-SN packet each) }
    procedure Feed(const ADatagram: TBytes; APlain: TList);
    /// The datagram for one packet of the application, encrypted.
    function Seal(const APlain: TBytes): TBytes;
    /// What is waiting to go to the peer (handshake, alerts); nil when nothing.
    function TakeOutput: TBytes;
    { Lets DTLS resend a handshake flight that got no answer (call it now and
      then while not Done) }
    procedure Tick;
    /// The peer's certificate, as for TLS.
    function PeerCertificate: TRALCertInfo;

    /// The handshake is over.
    property Done: boolean read FDone;
  end;

  /// Decides on a server certificate, with the same signature as TRALClient's event.
  TRALMQTTOnValidateCert = TRALOnValidateCert;

{ The certificate rule of TRALClient: AEvent decides when assigned; else a pin
  of ASSL.Pins that applies to AHost:APort must match; else ACert.Trusted
  (svNever: anything). Fills the Host and Port of the certificate }
function RALMQTTAcceptCertificate(ASender: TObject; ASSL: TRALClientSSL;
  AEvent: TRALOnValidateCert; var ACert: TRALCertInfo; const AHost: StringRAL;
  APort: IntegerRAL): boolean;
/// True when OpenSSL's libssl and libcrypto can be loaded here.
function RALMQTTTLSAvailable: boolean;
/// Name and version of the OpenSSL loaded, or why it could not be.
function RALMQTTTLSLibraryInfo: StringRAL;

implementation

uses
  {$IF Defined(POSIX) and not Defined(FPC)}
  Posix.Base,
  {$IFEND}
  RALSHA2_32, RALNetwork;

const
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_SYSCALL = 5;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_VERIFY_NONE = 0;
  SSL_VERIFY_PEER = 1;
  SSL_VERIFY_FAIL_IF_NO_PEER_CERT = 2;
  SSL_FILETYPE_PEM = 1;
  SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
  SSL_CTRL_SET_MIN_PROTO_VERSION = 123;
  SSL_CTRL_MODE = 33;
  SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER = $2;
  TLSEXT_NAMETYPE_host_name = 0;
  SSL_TLSEXT_ERR_OK = 0;
  SSL_TLSEXT_ERR_NOACK = 3;
  TLS1_2_VERSION = $0303;
  TLS1_3_VERSION = $0304;
  DTLS1_2_VERSION = $FEFD;
  BIO_NOCLOSE = 0;
  X509_V_OK = 0;

type
  { the two libraries; libcrypto first, libssl needs it }
  TRALMQTTCryptoLib = class(TRALExternalsLibraries)
  protected
    procedure LoadProcs; override;
  public
    constructor Create; override;
  end;

  TRALMQTTSSLLib = class(TRALExternalsLibraries)
  protected
    procedure LoadProcs; override;
  public
    constructor Create; override;
  end;

var
  GLock: TCriticalSection;
  GCrypto: TRALMQTTCryptoLib;
  GSSL: TRALMQTTSSLLib;
  GLoaded: boolean;
  GLoadError: string;

  { libssl }
  OPENSSL_init_ssl: function(opts: UInt64; settings: Pointer): Integer; cdecl;
  TLS_client_method: function: Pointer; cdecl;
  TLS_server_method: function: Pointer; cdecl;
  DTLS_client_method: function: Pointer; cdecl;
  DTLS_server_method: function: Pointer; cdecl;
  SSL_CTX_new: function(meth: Pointer): Pointer; cdecl;
  SSL_CTX_free: procedure(ctx: Pointer); cdecl;
  SSL_CTX_ctrl: function(ctx: Pointer; cmd: Integer; larg: NativeInt; parg: Pointer): NativeInt; cdecl;
  SSL_CTX_set_verify: procedure(ctx: Pointer; mode: Integer; cb: Pointer); cdecl;
  SSL_CTX_load_verify_locations: function(ctx: Pointer; CAfile, CApath: PAnsiChar): Integer; cdecl;
  SSL_CTX_set_default_verify_paths: function(ctx: Pointer): Integer; cdecl;
  SSL_CTX_use_certificate_chain_file: function(ctx: Pointer; f: PAnsiChar): Integer; cdecl;
  SSL_CTX_use_PrivateKey_file: function(ctx: Pointer; f: PAnsiChar; typ: Integer): Integer; cdecl;
  SSL_CTX_use_certificate: function(ctx: Pointer; x: Pointer): Integer; cdecl;
  SSL_CTX_use_PrivateKey: function(ctx: Pointer; pkey: Pointer): Integer; cdecl;
  SSL_CTX_check_private_key: function(ctx: Pointer): Integer; cdecl;
  SSL_CTX_set_default_passwd_cb: procedure(ctx: Pointer; cb: Pointer); cdecl;
  SSL_CTX_set_default_passwd_cb_userdata: procedure(ctx: Pointer; u: Pointer); cdecl;
  SSL_CTX_set_alpn_protos: function(ctx: Pointer; protos: PByte; len: Cardinal): Integer; cdecl;
  SSL_CTX_set_alpn_select_cb: procedure(ctx: Pointer; cb: Pointer; arg: Pointer); cdecl;
  SSL_CTX_get_cert_store: function(ctx: Pointer): Pointer; cdecl;
  SSL_CTX_set_client_CA_list: procedure(ctx: Pointer; list: Pointer); cdecl;
  SSL_load_client_CA_file: function(f: PAnsiChar): Pointer; cdecl;
  SSL_new: function(ctx: Pointer): Pointer; cdecl;
  SSL_free: procedure(ssl: Pointer); cdecl;
  SSL_set_fd: function(ssl: Pointer; fd: Integer): Integer; cdecl;
  SSL_set_bio: procedure(ssl, rbio, wbio: Pointer); cdecl;
  SSL_connect: function(ssl: Pointer): Integer; cdecl;
  SSL_accept: function(ssl: Pointer): Integer; cdecl;
  SSL_read: function(ssl: Pointer; buf: Pointer; num: Integer): Integer; cdecl;
  SSL_write: function(ssl: Pointer; const buf: Pointer; num: Integer): Integer; cdecl;
  SSL_get_error: function(ssl: Pointer; ret: Integer): Integer; cdecl;
  SSL_shutdown: function(ssl: Pointer): Integer; cdecl;
  SSL_pending: function(ssl: Pointer): Integer; cdecl;
  SSL_ctrl: function(ssl: Pointer; cmd: Integer; larg: NativeInt; parg: Pointer): NativeInt; cdecl;
  SSL_set1_host: function(ssl: Pointer; hostname: PAnsiChar): Integer; cdecl;
  SSL_get0_param: function(ssl: Pointer): Pointer; cdecl;
  SSL_get_verify_result: function(ssl: Pointer): NativeInt; cdecl;
  SSL_get_peer_certificate: function(ssl: Pointer): Pointer; cdecl;
  SSL_get1_peer_certificate: function(ssl: Pointer): Pointer; cdecl;
  SSL_get0_alpn_selected: procedure(ssl: Pointer; out data: PByte; out len: Cardinal); cdecl;
  SSL_get_version: function(ssl: Pointer): PAnsiChar; cdecl;
  SSL_set_alpn_protos: function(ssl: Pointer; protos: PByte; len: Cardinal): Integer; cdecl;
  SSL_set_connect_state: procedure(ssl: Pointer); cdecl;
  SSL_set_accept_state: procedure(ssl: Pointer); cdecl;
  SSL_do_handshake: function(ssl: Pointer): Integer; cdecl;

  { libcrypto }
  OpenSSL_version: function(t: Integer): PAnsiChar; cdecl;
  X509_free: procedure(x: Pointer); cdecl;
  i2d_X509: function(x: Pointer; out_: PPointer): Integer; cdecl;
  d2i_X509: function(px: PPointer; in_: PPointer; len: Integer): Pointer; cdecl;
  X509_get_subject_name: function(x: Pointer): Pointer; cdecl;
  X509_get_issuer_name: function(x: Pointer): Pointer; cdecl;
  X509_NAME_oneline: function(a: Pointer; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl;
  X509_get_serialNumber: function(x: Pointer): Pointer; cdecl;
  ASN1_INTEGER_to_BN: function(ai: Pointer; bn: Pointer): Pointer; cdecl;
  BN_bn2hex: function(a: Pointer): PAnsiChar; cdecl;
  BN_free: procedure(a: Pointer); cdecl;
  CRYPTO_free: procedure(p: Pointer; f: PAnsiChar; line: Integer); cdecl;
  X509_get0_notBefore: function(x: Pointer): Pointer; cdecl;
  X509_get0_notAfter: function(x: Pointer): Pointer; cdecl;
  ASN1_TIME_to_tm: function(s: Pointer; tm: Pointer): Integer; cdecl;
  X509_verify_cert_error_string: function(n: NativeInt): PAnsiChar; cdecl;
  X509_STORE_add_cert: function(store, x: Pointer): Integer; cdecl;
  X509_VERIFY_PARAM_set1_ip_asc: function(param: Pointer; ipasc: PAnsiChar): Integer; cdecl;
  BIO_new_mem_buf: function(buf: Pointer; len: Integer): Pointer; cdecl;
  BIO_new_dgram: function(fd, close_flag: Integer): Pointer; cdecl;
  BIO_new: function(typ: Pointer): Pointer; cdecl;
  BIO_s_mem: function: Pointer; cdecl;
  BIO_read: function(b: Pointer; data: Pointer; dlen: Integer): Integer; cdecl;
  BIO_write: function(b: Pointer; const data: Pointer; dlen: Integer): Integer; cdecl;
  BIO_ctrl: function(b: Pointer; cmd: Integer; larg: NativeInt; parg: Pointer): NativeInt; cdecl;
  BIO_free: function(bio: Pointer): Integer; cdecl;
  PEM_read_bio_X509: function(bp: Pointer; x: PPointer; cb, u: Pointer): Pointer; cdecl;
  PEM_read_bio_PrivateKey: function(bp: Pointer; x: PPointer; cb, u: Pointer): Pointer; cdecl;
  EVP_PKEY_free: procedure(pkey: Pointer); cdecl;
  ERR_get_error: function: Cardinal; cdecl;
  ERR_error_string_n: procedure(e: Cardinal; buf: PAnsiChar; len: NativeUInt); cdecl;
  ERR_clear_error: procedure; cdecl;

{$IFDEF RALWindows}
const
  cCrypt32 = 'crypt32.dll';

type
  PCertContext = ^TCertContext;
  TCertContext = record
    dwCertEncodingType: Cardinal;
    pbCertEncoded: PByte;
    cbCertEncoded: Cardinal;
    pCertInfo: Pointer;
    hCertStore: Pointer;
  end;

function CertOpenSystemStoreW(hProv: NativeUInt; szSubsystemProtocol: PWideChar): Pointer; stdcall;
  external cCrypt32 name 'CertOpenSystemStoreW';
function CertEnumCertificatesInStore(hCertStore: Pointer; pPrevCertContext: PCertContext): PCertContext; stdcall;
  external cCrypt32 name 'CertEnumCertificatesInStore';
function CertCloseStore(hCertStore: Pointer; dwFlags: Cardinal): LongBool; stdcall;
  external cCrypt32 name 'CertCloseStore';
{$ENDIF}

{ TRALMQTTCryptoLib }

constructor TRALMQTTCryptoLib.Create;
begin
  inherited Create;
  {$IFDEF RALWindows}
    {$IFDEF CPU64}
    AddLibrary('libcrypto-3-x64.dll');
    AddLibrary('libcrypto-1_1-x64.dll');
    {$ELSE}
    AddLibrary('libcrypto-3.dll');
    AddLibrary('libcrypto-1_1.dll');
    {$ENDIF}
  {$ELSE}
    {$IF Defined(DARWIN) or Defined(MACOS) or Defined(IOS)}
    AddLibrary('libcrypto.3.dylib');
    AddLibrary('libcrypto.1.1.dylib');
    AddLibrary('libcrypto.dylib');
    {$ELSE}
    AddLibrary('libcrypto.so.3');
    AddLibrary('libcrypto.so.1.1');
    AddLibrary('libcrypto.so');
    {$IFEND}
  {$ENDIF}
  LoadLibrary;
end;

procedure TRALMQTTCryptoLib.LoadProcs;
begin
  LoadProc(@OpenSSL_version, 'OpenSSL_version');
  LoadProc(@X509_free, 'X509_free');
  LoadProc(@i2d_X509, 'i2d_X509');
  LoadProc(@d2i_X509, 'd2i_X509');
  LoadProc(@X509_get_subject_name, 'X509_get_subject_name');
  LoadProc(@X509_get_issuer_name, 'X509_get_issuer_name');
  LoadProc(@X509_NAME_oneline, 'X509_NAME_oneline');
  LoadProc(@X509_get_serialNumber, 'X509_get_serialNumber');
  LoadProc(@ASN1_INTEGER_to_BN, 'ASN1_INTEGER_to_BN');
  LoadProc(@BN_bn2hex, 'BN_bn2hex');
  LoadProc(@BN_free, 'BN_free');
  LoadProc(@CRYPTO_free, 'CRYPTO_free');
  LoadProc(@X509_get0_notBefore, 'X509_get0_notBefore');
  LoadProc(@X509_get0_notAfter, 'X509_get0_notAfter');
  LoadProc(@ASN1_TIME_to_tm, 'ASN1_TIME_to_tm');
  LoadProc(@X509_verify_cert_error_string, 'X509_verify_cert_error_string');
  LoadProc(@X509_STORE_add_cert, 'X509_STORE_add_cert');
  LoadProc(@X509_VERIFY_PARAM_set1_ip_asc, 'X509_VERIFY_PARAM_set1_ip_asc');
  LoadProc(@BIO_new_mem_buf, 'BIO_new_mem_buf');
  LoadProc(@BIO_new_dgram, 'BIO_new_dgram');
  LoadProc(@BIO_new, 'BIO_new');
  LoadProc(@BIO_s_mem, 'BIO_s_mem');
  LoadProc(@BIO_read, 'BIO_read');
  LoadProc(@BIO_write, 'BIO_write');
  LoadProc(@BIO_ctrl, 'BIO_ctrl');
  LoadProc(@BIO_free, 'BIO_free');
  LoadProc(@PEM_read_bio_X509, 'PEM_read_bio_X509');
  LoadProc(@PEM_read_bio_PrivateKey, 'PEM_read_bio_PrivateKey');
  LoadProc(@EVP_PKEY_free, 'EVP_PKEY_free');
  LoadProc(@ERR_get_error, 'ERR_get_error');
  LoadProc(@ERR_error_string_n, 'ERR_error_string_n');
  LoadProc(@ERR_clear_error, 'ERR_clear_error');
end;

{ TRALMQTTSSLLib }

constructor TRALMQTTSSLLib.Create;
begin
  inherited Create;
  {$IFDEF RALWindows}
    {$IFDEF CPU64}
    AddLibrary('libssl-3-x64.dll');
    AddLibrary('libssl-1_1-x64.dll');
    {$ELSE}
    AddLibrary('libssl-3.dll');
    AddLibrary('libssl-1_1.dll');
    {$ENDIF}
  {$ELSE}
    {$IF Defined(DARWIN) or Defined(MACOS) or Defined(IOS)}
    AddLibrary('libssl.3.dylib');
    AddLibrary('libssl.1.1.dylib');
    AddLibrary('libssl.dylib');
    {$ELSE}
    AddLibrary('libssl.so.3');
    AddLibrary('libssl.so.1.1');
    AddLibrary('libssl.so');
    {$IFEND}
  {$ENDIF}
  LoadLibrary;
end;

procedure TRALMQTTSSLLib.LoadProcs;
begin
  LoadProc(@OPENSSL_init_ssl, 'OPENSSL_init_ssl');
  LoadProc(@TLS_client_method, 'TLS_client_method');
  LoadProc(@TLS_server_method, 'TLS_server_method');
  LoadProc(@DTLS_client_method, 'DTLS_client_method');
  LoadProc(@DTLS_server_method, 'DTLS_server_method');
  LoadProc(@SSL_CTX_new, 'SSL_CTX_new');
  LoadProc(@SSL_CTX_free, 'SSL_CTX_free');
  LoadProc(@SSL_CTX_ctrl, 'SSL_CTX_ctrl');
  LoadProc(@SSL_CTX_set_verify, 'SSL_CTX_set_verify');
  LoadProc(@SSL_CTX_load_verify_locations, 'SSL_CTX_load_verify_locations');
  LoadProc(@SSL_CTX_set_default_verify_paths, 'SSL_CTX_set_default_verify_paths');
  LoadProc(@SSL_CTX_use_certificate_chain_file, 'SSL_CTX_use_certificate_chain_file');
  LoadProc(@SSL_CTX_use_PrivateKey_file, 'SSL_CTX_use_PrivateKey_file');
  LoadProc(@SSL_CTX_use_certificate, 'SSL_CTX_use_certificate');
  LoadProc(@SSL_CTX_use_PrivateKey, 'SSL_CTX_use_PrivateKey');
  LoadProc(@SSL_CTX_check_private_key, 'SSL_CTX_check_private_key');
  LoadProc(@SSL_CTX_set_default_passwd_cb, 'SSL_CTX_set_default_passwd_cb');
  LoadProc(@SSL_CTX_set_default_passwd_cb_userdata, 'SSL_CTX_set_default_passwd_cb_userdata');
  LoadProc(@SSL_CTX_set_alpn_protos, 'SSL_CTX_set_alpn_protos');
  LoadProc(@SSL_CTX_set_alpn_select_cb, 'SSL_CTX_set_alpn_select_cb');
  LoadProc(@SSL_CTX_get_cert_store, 'SSL_CTX_get_cert_store');
  LoadProc(@SSL_CTX_set_client_CA_list, 'SSL_CTX_set_client_CA_list');
  LoadProc(@SSL_load_client_CA_file, 'SSL_load_client_CA_file');
  LoadProc(@SSL_new, 'SSL_new');
  LoadProc(@SSL_free, 'SSL_free');
  LoadProc(@SSL_set_fd, 'SSL_set_fd');
  LoadProc(@SSL_set_bio, 'SSL_set_bio');
  LoadProc(@SSL_connect, 'SSL_connect');
  LoadProc(@SSL_accept, 'SSL_accept');
  LoadProc(@SSL_read, 'SSL_read');
  LoadProc(@SSL_write, 'SSL_write');
  LoadProc(@SSL_get_error, 'SSL_get_error');
  LoadProc(@SSL_shutdown, 'SSL_shutdown');
  LoadProc(@SSL_pending, 'SSL_pending');
  LoadProc(@SSL_ctrl, 'SSL_ctrl');
  LoadProc(@SSL_set1_host, 'SSL_set1_host');
  LoadProc(@SSL_get0_param, 'SSL_get0_param');
  LoadProc(@SSL_get_verify_result, 'SSL_get_verify_result');
  LoadProc(@SSL_get_peer_certificate, 'SSL_get_peer_certificate');
  LoadProc(@SSL_get1_peer_certificate, 'SSL_get1_peer_certificate');
  LoadProc(@SSL_get0_alpn_selected, 'SSL_get0_alpn_selected');
  LoadProc(@SSL_get_version, 'SSL_get_version');
  LoadProc(@SSL_set_alpn_protos, 'SSL_set_alpn_protos');
  LoadProc(@SSL_set_connect_state, 'SSL_set_connect_state');
  LoadProc(@SSL_set_accept_state, 'SSL_set_accept_state');
  LoadProc(@SSL_do_handshake, 'SSL_do_handshake');
end;

{$IFNDEF RALWindows}
const
  cSIGPIPE = 13; // the same on Linux, Android, the BSDs and Apple
  {$IFDEF FPC}
  cLibC = 'c';
  {$ELSE}
  cLibC = libc;
  {$ENDIF}

function sys_signal(ASignal: Integer; AHandler: Pointer): Pointer; cdecl;
  external cLibC name {$IFNDEF FPC}_PU + {$ENDIF}'signal';

{ OpenSSL writes to the socket itself, without MSG_NOSIGNAL: a peer that
  closed would kill the process with SIGPIPE. Ignored here, unless the
  application installed a handler of its own }
procedure IgnoreSIGPIPE;
var
  vOld: Pointer;
begin
  vOld := sys_signal(cSIGPIPE, Pointer(1)); // SIG_IGN
  if (vOld <> nil) and (vOld <> Pointer(1)) then
    sys_signal(cSIGPIPE, vOld);
end;
{$ENDIF}

{ loads both libraries once; True when everything the code calls is there }
function LoadOpenSSL: boolean;
begin
  GLock.Acquire;
  try
    if not GLoaded then
    begin
      GLoaded := True;
      try
        GCrypto := TRALMQTTCryptoLib.Create;
        GSSL := TRALMQTTSSLLib.Create;
        if GCrypto.LibraryHandle = 0 then
          GLoadError := emMQTTNoLibCrypto
        else if GSSL.LibraryHandle = 0 then
          GLoadError := emMQTTNoLibSSL
        { 1.1.1 or later: TLS_client_method and SSL_set1_host came in 1.1.0,
          ASN1_TIME_to_tm in 1.1.1 }
        else if (not Assigned(TLS_client_method)) or (not Assigned(SSL_set1_host)) or
                (not Assigned(ASN1_TIME_to_tm)) then
          GLoadError := emMQTTOldOpenSSL
        else
        begin
          // 3.0 renamed it; either one does
          if not Assigned(SSL_get1_peer_certificate) then
            SSL_get1_peer_certificate := SSL_get_peer_certificate;
          if Assigned(OPENSSL_init_ssl) then
            OPENSSL_init_ssl(0, nil);
          {$IFNDEF RALWindows}
          IgnoreSIGPIPE;
          {$ENDIF}
        end;
      except
        on e: Exception do
          GLoadError := e.Message;
      end;
    end;
    Result := GLoadError = '';
  finally
    GLock.Release;
  end;
end;

function RALMQTTTLSAvailable: boolean;
begin
  Result := LoadOpenSSL;
end;

function RALMQTTTLSLibraryInfo: StringRAL;
begin
  if LoadOpenSSL then
  begin
    if Assigned(OpenSSL_version) then
      Result := StringRAL(OpenSSL_version(0))
    else
      Result := StringRAL(GSSL.LibraryName);
  end
  else
    Result := StringRAL(GLoadError);
end;

{ the errors OpenSSL queued, as text, and the queue emptied }
function OpenSSLErrors: string;
var
  vCode: Cardinal;
  vBuf: array[0..255] of AnsiChar;
begin
  Result := '';
  if not Assigned(ERR_get_error) then
    Exit;
  repeat
    vCode := ERR_get_error();
    if vCode = 0 then
      Break;
    ERR_error_string_n(vCode, @vBuf[0], SizeOf(vBuf));
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + string(AnsiString(PAnsiChar(@vBuf[0])));
  until False;
end;

procedure RaiseTLS(const AWhat: string);
var
  vDetail: string;
begin
  vDetail := OpenSSLErrors;
  if vDetail = '' then
    vDetail := '-';
  raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
    Format(emMQTTTLSError, [AWhat, vDetail]));
end;

{ ALPN on the wire: each name preceded by its length }
function ALPNWire(const AList: StringRAL): TBytes;
var
  vParts: TStringList;
  vInt, vPos: IntegerRAL;
  vName: AnsiString;
begin
  Result := nil;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ',';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := string(AList);
    vPos := 0;
    for vInt := 0 to vParts.Count - 1 do
    begin
      vName := AnsiString(Trim(vParts[vInt]));
      if (vName = '') or (Length(vName) > 255) then
        Continue;
      SetLength(Result, vPos + 1 + Length(vName));
      Result[vPos] := Length(vName);
      Move(vName[1], Result[vPos + 1], Length(vName));
      Inc(vPos, 1 + Length(vName));
    end;
  finally
    vParts.Free;
  end;
end;

function PasswordCallback(buf: PAnsiChar; size, rwflag: Integer; userdata: Pointer): Integer; cdecl;
var
  vPassword: PAnsiChar;
begin
  vPassword := userdata;
  Result := 0;
  if vPassword = nil then
    Exit;
  while vPassword[Result] <> #0 do
    Inc(Result);
  if Result > size then
    Result := size;
  Move(vPassword^, buf^, Result);
end;

{ server side of ALPN: the first of the server's protocols the client offered }
function ALPNSelect(ssl: Pointer; out_: PPointer; outlen: PByte; in_: PByte;
  inlen: Cardinal; arg: Pointer): Integer; cdecl;
var
  vMine: TBytes;
  vPos, vOffer: Cardinal;
  vMineLen, vOfferLen: Byte;
begin
  Result := SSL_TLSEXT_ERR_NOACK;
  vMine := ALPNWire(TRALMQTTTLSContext(arg).ALPN);
  vPos := 0;
  while vPos < Cardinal(Length(vMine)) do
  begin
    vMineLen := vMine[vPos];
    vOffer := 0;
    while vOffer < inlen do
    begin
      vOfferLen := in_[vOffer];
      if (vOfferLen = vMineLen) and (vOffer + 1 + vOfferLen <= inlen) and
         CompareMem(@in_[vOffer + 1], @vMine[vPos + 1], vMineLen) then
      begin
        out_^ := @in_[vOffer + 1];
        outlen^ := vOfferLen;
        Result := SSL_TLSEXT_ERR_OK;
        Exit;
      end;
      Inc(vOffer, 1 + vOfferLen);
    end;
    Inc(vPos, 1 + vMineLen);
  end;
end;

function RALMQTTAcceptCertificate(ASender: TObject; ASSL: TRALClientSSL;
  AEvent: TRALOnValidateCert; var ACert: TRALCertInfo; const AHost: StringRAL;
  APort: IntegerRAL): boolean;
var
  vInt, vEquals, vPinPort: IntegerRAL;
  vLine, vPinHost, vFingerprint: StringRAL;
  vApplies, vMatches: boolean;
begin
  ACert.Host := AHost;
  ACert.Port := APort;
  if Assigned(AEvent) then
  begin
    Result := AEvent(ASender, ACert);
    Exit;
  end;

  vApplies := False;
  vMatches := False;
  if ASSL <> nil then
    for vInt := 0 to ASSL.Pins.Count - 1 do
    begin
      vLine := StringRAL(ASSL.Pins.Strings[vInt]);
      vEquals := Pos(StringRAL('='), vLine);
      if vEquals > 0 then
      begin
        RALSplitHostPort(Copy(vLine, 1, vEquals - 1), vPinHost, vPinPort);
        if not (SameText(string(vPinHost), string(AHost)) and
                ((vPinPort = 0) or (vPinPort = APort))) then
          Continue;
      end;
      vApplies := True;
      vFingerprint := RALNormalizeFingerprint(Copy(vLine, vEquals + 1, MaxInt));
      if (ACert.Fingerprint <> '') and (vFingerprint = ACert.Fingerprint) then
      begin
        vMatches := True;
        Break;
      end;
    end;

  if vApplies then
    Result := vMatches
  else if (ASSL <> nil) and (ASSL.Verify = svNever) then
    Result := True
  else
    Result := ACert.Trusted;
end;

{ TRALMQTTSSL }

constructor TRALMQTTSSL.Create;
begin
  inherited Create;
  FMinVersion := mtv12;
end;

procedure TRALMQTTSSL.Assign(ASource: TPersistent);
var
  vSource: TRALMQTTSSL;
begin
  inherited Assign(ASource);
  if ASource is TRALMQTTSSL then
  begin
    vSource := TRALMQTTSSL(ASource);
    FALPN := vSource.ALPN;
    FCAFile := vSource.CAFile;
    FCertFile := vSource.CertFile;
    FEnabled := vSource.Enabled;
    FKeyFile := vSource.KeyFile;
    FKeyPassword := vSource.KeyPassword;
    FMinVersion := vSource.MinVersion;
    FServerName := vSource.ServerName;
  end;
end;

{ TRALMQTTTLSContext }

constructor TRALMQTTTLSContext.Create(ARole: TRALMQTTTLSRole; ADatagram: boolean);
var
  vMethod: Pointer;
begin
  inherited Create;
  FRole := ARole;
  FDatagram := ADatagram;
  if not LoadOpenSSL then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, GLoadError);
  if ADatagram then
  begin
    if ARole = mtrClient then
      vMethod := DTLS_client_method()
    else
      vMethod := DTLS_server_method();
  end
  else if ARole = mtrClient then
    vMethod := TLS_client_method()
  else
    vMethod := TLS_server_method();
  FContext := SSL_CTX_new(vMethod);
  if FContext = nil then
    RaiseTLS('SSL_CTX_new');
  // the verdict is taken after the handshake, by the rule of TRALClient
  SSL_CTX_set_verify(FContext, SSL_VERIFY_NONE, nil);
  { a write that has to wait is retried from wherever the caller keeps the
    bytes now, not from the address of the first try }
  SSL_CTX_ctrl(FContext, SSL_CTRL_MODE, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER, nil);
  SetMinVersion(mtv12);
  SSL_CTX_set_default_passwd_cb(FContext, @PasswordCallback);
  if ARole = mtrServer then
    SSL_CTX_set_alpn_select_cb(FContext, @ALPNSelect, Self);
end;

destructor TRALMQTTTLSContext.Destroy;
begin
  if FContext <> nil then
    SSL_CTX_free(FContext);
  inherited Destroy;
end;

procedure TRALMQTTTLSContext.LoadCAFile(const AFile: StringRAL);
begin
  if AFile = '' then
    Exit;
  if SSL_CTX_load_verify_locations(FContext, PAnsiChar(AnsiString(AFile)), nil) <> 1 then
    RaiseTLS(Format(emMQTTTLSFile, [string(AFile)]));
end;

procedure TRALMQTTTLSContext.LoadSystemCAs;
{$IFDEF RALWindows}
var
  vStore: Pointer;
  vCert: PCertContext;
  vData: PByte;
  vX509: Pointer;
  vName: WideString;
{$ENDIF}
begin
  SSL_CTX_set_default_verify_paths(FContext);
  {$IFDEF RALWindows}
  { OpenSSL on Windows knows no store of its own: the system's root
    certificates are copied into the context }
  vName := 'ROOT';
  vStore := CertOpenSystemStoreW(0, PWideChar(vName));
  if vStore = nil then
    Exit;
  try
    vCert := CertEnumCertificatesInStore(vStore, nil);
    while vCert <> nil do
    begin
      vData := vCert^.pbCertEncoded;
      vX509 := d2i_X509(nil, @vData, vCert^.cbCertEncoded);
      if vX509 <> nil then
      begin
        X509_STORE_add_cert(SSL_CTX_get_cert_store(FContext), vX509);
        X509_free(vX509);
      end;
      vCert := CertEnumCertificatesInStore(vStore, vCert);
    end;
    ERR_clear_error; // duplicates already in the store are not a failure
  finally
    CertCloseStore(vStore, 0);
  end;
  {$ELSE}
  {$IF Defined(ANDROID) or Defined(Android32) or Defined(Android64)}
  // the system store of Android: one file per certificate, by hash
  SSL_CTX_load_verify_locations(FContext, nil, '/system/etc/security/cacerts');
  {$IFEND}
  {$ENDIF}
end;

procedure TRALMQTTTLSContext.RequestClientCertificate(ARequire: boolean;
  const ACAFile: StringRAL);
var
  vMode: Integer;
begin
  vMode := SSL_VERIFY_PEER;
  if ARequire then
    vMode := vMode or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
  if ACAFile <> '' then
  begin
    LoadCAFile(ACAFile);
    SSL_CTX_set_client_CA_list(FContext, SSL_load_client_CA_file(PAnsiChar(AnsiString(ACAFile))));
  end;
  SSL_CTX_set_verify(FContext, vMode, nil);
end;

procedure TRALMQTTTLSContext.SetMinVersion(AVersion: TRALMQTTTLSVersion);
var
  vVersion: NativeInt;
begin
  if FDatagram then
    vVersion := DTLS1_2_VERSION
  else if AVersion = mtv13 then
    vVersion := TLS1_3_VERSION
  else
    vVersion := TLS1_2_VERSION;
  SSL_CTX_ctrl(FContext, SSL_CTRL_SET_MIN_PROTO_VERSION, vVersion, nil);
end;

procedure TRALMQTTTLSContext.UseCertificateFiles(const ACertFile, AKeyFile,
  APassword: StringRAL);
var
  vKey: StringRAL;
begin
  if ACertFile = '' then
    Exit;
  FKeyPassword := AnsiString(APassword);
  if FKeyPassword <> '' then
    SSL_CTX_set_default_passwd_cb_userdata(FContext, PAnsiChar(FKeyPassword))
  else
    SSL_CTX_set_default_passwd_cb_userdata(FContext, nil);
  if SSL_CTX_use_certificate_chain_file(FContext, PAnsiChar(AnsiString(ACertFile))) <> 1 then
    RaiseTLS(Format(emMQTTTLSFile, [string(ACertFile)]));
  vKey := AKeyFile;
  if vKey = '' then
    vKey := ACertFile; // key and certificate in one PEM
  if SSL_CTX_use_PrivateKey_file(FContext, PAnsiChar(AnsiString(vKey)), SSL_FILETYPE_PEM) <> 1 then
    RaiseTLS(Format(emMQTTTLSFile, [string(vKey)]));
  if SSL_CTX_check_private_key(FContext) <> 1 then
    RaiseTLS(emMQTTTLSKeyMismatch);
end;

procedure TRALMQTTTLSContext.UseCertificatePEM(const ACertPEM, AKeyPEM: StringRAL);
var
  vBio: Pointer;
  vCert, vKey: Pointer;
begin
  vBio := BIO_new_mem_buf(Pointer(ACertPEM), Length(ACertPEM));
  try
    vCert := PEM_read_bio_X509(vBio, nil, nil, nil);
    if vCert = nil then
      RaiseTLS('PEM_read_bio_X509');
    try
      if SSL_CTX_use_certificate(FContext, vCert) <> 1 then
        RaiseTLS('SSL_CTX_use_certificate');
    finally
      X509_free(vCert);
    end;
  finally
    BIO_free(vBio);
  end;
  vBio := BIO_new_mem_buf(Pointer(AKeyPEM), Length(AKeyPEM));
  try
    vKey := PEM_read_bio_PrivateKey(vBio, nil, nil, nil);
    if vKey = nil then
      RaiseTLS('PEM_read_bio_PrivateKey');
    try
      if SSL_CTX_use_PrivateKey(FContext, vKey) <> 1 then
        RaiseTLS('SSL_CTX_use_PrivateKey');
    finally
      EVP_PKEY_free(vKey);
    end;
  finally
    BIO_free(vBio);
  end;
  if SSL_CTX_check_private_key(FContext) <> 1 then
    RaiseTLS(emMQTTTLSKeyMismatch);
end;

{ TRALMQTTTLSChannel }

constructor TRALMQTTTLSChannel.Create(AInner: TRALMQTTChannel;
  AContext: TRALMQTTTLSContext; AOwnsInner: boolean);
var
  vBio: Pointer;
  vWire: TBytes;
begin
  inherited Create(AInner, AOwnsInner);
  FContext := AContext;
  FSSL := SSL_new(AContext.Handle);
  if FSSL = nil then
    RaiseTLS('SSL_new');
  if AContext.FDatagram then
  begin
    // DTLS keeps datagram boundaries: a datagram BIO, not a stream one
    vBio := BIO_new_dgram(Integer(AInner.Socket.Handle), BIO_NOCLOSE);
    if vBio = nil then
      RaiseTLS('BIO_new_dgram');
    SSL_set_bio(FSSL, vBio, vBio);
  end
  else if SSL_set_fd(FSSL, Integer(AInner.Socket.Handle)) <> 1 then
    RaiseTLS('SSL_set_fd');
  if (AContext.Role = mtrClient) and (AContext.ALPN <> '') then
  begin
    vWire := ALPNWire(AContext.ALPN);
    if vWire <> nil then
      SSL_set_alpn_protos(FSSL, @vWire[0], Length(vWire));
  end;
  FWant := RALMQTTPollIn;
end;

destructor TRALMQTTTLSChannel.Destroy;
begin
  if FSSL <> nil then
    SSL_free(FSSL);
  inherited Destroy;
end;

function TRALMQTTTLSChannel.CheckIO(ARes: Integer; const AWhat: string): IntegerRAL;
var
  vError: Integer;
begin
  vError := SSL_get_error(FSSL, ARes);
  case vError of
    SSL_ERROR_WANT_READ:
    begin
      FWant := RALMQTTPollIn;
      Result := RALMQTTWouldBlock;
    end;
    SSL_ERROR_WANT_WRITE:
    begin
      FWant := RALMQTTPollOut;
      Result := RALMQTTWouldBlock;
    end;
    SSL_ERROR_ZERO_RETURN:
      Result := 0; // the peer closed TLS (close_notify)
    SSL_ERROR_SYSCALL:
      // an EOF without close_notify reads as a plain close
      Result := 0;
  else
    RaiseTLS(AWhat);
    Result := 0;
  end;
end;

procedure TRALMQTTTLSChannel.Close;
begin
  if (FSSL <> nil) and FDone then
  try
    SSL_shutdown(FSSL); // one close_notify, without waiting for the answer
  except
    // the link may already be gone
  end;
  inherited Close;
end;

procedure TRALMQTTTLSChannel.Handshake(const AServerName: StringRAL; ATimeout: IntegerRAL);
var
  vHost: AnsiString;
  vRes: Integer;
  vDeadline, vNow: Int64RAL;
begin
  vHost := AnsiString(AServerName);
  if (Length(vHost) > 1) and (vHost[1] = '[') then
    vHost := Copy(vHost, 2, Length(vHost) - 2);
  if vHost <> '' then
  begin
    if RALIsIPAddress(StringRAL(vHost), rimIPv4) or RALIsIPAddress(StringRAL(vHost), rimIPv6) then
      // an address is no name for SNI; it is checked against the IP of the certificate
      X509_VERIFY_PARAM_set1_ip_asc(SSL_get0_param(FSSL), PAnsiChar(vHost))
    else
    begin
      SSL_ctrl(FSSL, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, PAnsiChar(vHost));
      SSL_set1_host(FSSL, PAnsiChar(vHost));
    end;
  end;
  vDeadline := RALMQTTNow + ATimeout;
  repeat
    vRes := SSL_connect(FSSL);
    if vRes = 1 then
      Break;
    if CheckIO(vRes, 'SSL_connect') <> RALMQTTWouldBlock then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTTLSClosed);
    vNow := RALMQTTNow;
    if vNow >= vDeadline then
      raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError,
        Format(emMQTTTLSTimeout, [ATimeout]));
    Socket.WaitFor(FWant, vDeadline - vNow);
  until False;
  FDone := True;
  FWant := RALMQTTPollIn;
end;

function TRALMQTTTLSChannel.HandshakeStep: boolean;
var
  vRes: Integer;
begin
  Result := FDone;
  if Result then
    Exit;
  vRes := SSL_accept(FSSL);
  if vRes = 1 then
  begin
    FDone := True;
    FWant := RALMQTTPollIn;
    Result := True;
    Exit;
  end;
  if CheckIO(vRes, 'SSL_accept') <> RALMQTTWouldBlock then
    raise ERALMQTTError.CreateReason(MQTTRC_UnspecifiedError, emMQTTTLSClosed);
end;

function TRALMQTTTLSChannel.HasBuffered: boolean;
begin
  Result := FDone and (SSL_pending(FSSL) > 0);
end;

function CertificateDER(ASSL: Pointer): TBytes;
var
  vCert: Pointer;
  vLen: Integer;
  vPtr: Pointer;
begin
  Result := nil;
  vCert := SSL_get1_peer_certificate(ASSL);
  if vCert = nil then
    Exit;
  try
    vLen := i2d_X509(vCert, nil);
    if vLen <= 0 then
      Exit;
    SetLength(Result, vLen);
    vPtr := @Result[0];
    i2d_X509(vCert, @vPtr);
  finally
    X509_free(vCert);
  end;
end;

function TRALMQTTTLSChannel.PeerCertificateDER: TBytes;
begin
  Result := CertificateDER(FSSL);
end;

{ the peer certificate of ASSL as TRALClient describes it }
function CertificateOf(ASSL: Pointer): TRALCertInfo;
type
  TTm = record
    tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_yday, tm_isdst: Integer;
    tm_pad: array[0..7] of Integer; // tm_gmtoff and tm_zone on some systems
  end;
var
  vCert, vBN: Pointer;
  vBuf: array[0..511] of AnsiChar;
  vHex: PAnsiChar;
  vDER: TBytes;
  vSHA: TRALSHA2_32;
  vDigest: TBytes;
  vInt: IntegerRAL;
  vResult: NativeInt;
  vTm: TTm;

  function TmToDate(const ATm: TTm): TDateTime;
  begin
    Result := EncodeDate(ATm.tm_year + 1900, ATm.tm_mon + 1, ATm.tm_mday) +
              EncodeTime(ATm.tm_hour, ATm.tm_min, ATm.tm_sec, 0);
  end;

begin
  Result := RALEmptyCertInfo;
  vDER := CertificateDER(ASSL);
  if vDER <> nil then
  begin
    vSHA := TRALSHA2_32.Create;
    try
      vSHA.HashBegin;
      vSHA.HashUpdate(@vDER[0], Length(vDER));
      vDigest := vSHA.HashEnd;
      Result.Fingerprint := '';
      for vInt := 0 to High(vDigest) do
        Result.Fingerprint := Result.Fingerprint + StringRAL(IntToHex(vDigest[vInt], 2));
    finally
      vSHA.Free;
    end;
  end;
  vCert := SSL_get1_peer_certificate(ASSL);
  if vCert <> nil then
  try
    FillChar(vBuf, SizeOf(vBuf), 0);
    X509_NAME_oneline(X509_get_subject_name(vCert), @vBuf[0], SizeOf(vBuf));
    Result.Subject := StringRAL(AnsiString(PAnsiChar(@vBuf[0])));
    FillChar(vBuf, SizeOf(vBuf), 0);
    X509_NAME_oneline(X509_get_issuer_name(vCert), @vBuf[0], SizeOf(vBuf));
    Result.Issuer := StringRAL(AnsiString(PAnsiChar(@vBuf[0])));
    vBN := ASN1_INTEGER_to_BN(X509_get_serialNumber(vCert), nil);
    if vBN <> nil then
    begin
      vHex := BN_bn2hex(vBN);
      if vHex <> nil then
      begin
        Result.SerialNumber := StringRAL(AnsiString(vHex));
        CRYPTO_free(vHex, nil, 0);
      end;
      BN_free(vBN);
    end;
    FillChar(vTm, SizeOf(vTm), 0);
    if ASN1_TIME_to_tm(X509_get0_notBefore(vCert), @vTm) = 1 then
      Result.NotBefore := TmToDate(vTm);
    FillChar(vTm, SizeOf(vTm), 0);
    if ASN1_TIME_to_tm(X509_get0_notAfter(vCert), @vTm) = 1 then
      Result.NotAfter := TmToDate(vTm);
  finally
    X509_free(vCert);
  end;
  vResult := SSL_get_verify_result(ASSL);
  Result.Trusted := (vResult = X509_V_OK) and (vDER <> nil);
  if vDER = nil then
    Result.Error := StringRAL(emMQTTTLSNoCertificate)
  else if vResult <> X509_V_OK then
    Result.Error := StringRAL(AnsiString(X509_verify_cert_error_string(vResult)));
end;

function TRALMQTTTLSChannel.PeerCertificate: TRALCertInfo;
begin
  Result := CertificateOf(FSSL);
end;

function TRALMQTTTLSChannel.Read(var ABuffer; ACount: IntegerRAL): IntegerRAL;
var
  vRes: Integer;
begin
  if not FDone then
  begin
    Result := RALMQTTWouldBlock;
    Exit;
  end;
  vRes := SSL_read(FSSL, @ABuffer, ACount);
  if vRes > 0 then
  begin
    FWant := RALMQTTPollIn;
    Result := vRes;
  end
  else
    Result := CheckIO(vRes, 'SSL_read');
end;

function TRALMQTTTLSChannel.SelectedALPN: StringRAL;
var
  vData: PByte;
  vLen: Cardinal;
begin
  Result := '';
  vData := nil;
  vLen := 0;
  SSL_get0_alpn_selected(FSSL, vData, vLen);
  if (vData <> nil) and (vLen > 0) then
  begin
    SetLength(Result, vLen);
    Move(vData^, Pointer(Result)^, vLen);
  end;
end;

function TRALMQTTTLSChannel.VersionName: StringRAL;
begin
  Result := StringRAL(AnsiString(SSL_get_version(FSSL)));
end;

function TRALMQTTTLSChannel.WantedEvents: Word;
begin
  Result := FWant;
end;

function TRALMQTTTLSChannel.Write(const ABuffer; ACount: IntegerRAL): IntegerRAL;
var
  vRes: Integer;
begin
  if not FDone then
  begin
    Result := RALMQTTWouldBlock;
    Exit;
  end;
  if ACount = 0 then
  begin
    Result := 0;
    Exit;
  end;
  vRes := SSL_write(FSSL, @ABuffer, ACount);
  if vRes > 0 then
    Result := vRes
  else
  begin
    Result := CheckIO(vRes, 'SSL_write');
    if Result = 0 then
      raise ERALMQTTSocketError.CreateCode(0, emMQTTConnectionClosed);
  end;
end;

{ TRALMQTTDTLSSession }

const
  BIO_CTRL_PENDING = 10;
  SSL_CTRL_SET_MTU = 17;
  DTLS_CTRL_HANDLE_TIMEOUT = 74;

constructor TRALMQTTDTLSSession.Create(AContext: TRALMQTTTLSContext; AServer: boolean;
  const AServerName: StringRAL);
var
  vHost: AnsiString;
begin
  inherited Create;
  FContext := AContext;
  FServer := AServer;
  FSSL := SSL_new(AContext.Handle);
  if FSSL = nil then
    RaiseTLS('SSL_new');
  FRead := BIO_new(BIO_s_mem());
  FWrite := BIO_new(BIO_s_mem());
  SSL_set_bio(FSSL, FRead, FWrite);
  // memory has no MTU to ask: the usual Ethernet payload, less headers
  SSL_ctrl(FSSL, SSL_CTRL_SET_MTU, 1200, nil);
  if AServer then
    SSL_set_accept_state(FSSL)
  else
  begin
    SSL_set_connect_state(FSSL);
    vHost := AnsiString(AServerName);
    if vHost <> '' then
    begin
      if RALIsIPAddress(StringRAL(vHost), rimIPv4) or RALIsIPAddress(StringRAL(vHost), rimIPv6) then
        X509_VERIFY_PARAM_set1_ip_asc(SSL_get0_param(FSSL), PAnsiChar(vHost))
      else
      begin
        SSL_ctrl(FSSL, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, PAnsiChar(vHost));
        SSL_set1_host(FSSL, PAnsiChar(vHost));
      end;
    end;
    Step; // the ClientHello
  end;
end;

destructor TRALMQTTDTLSSession.Destroy;
begin
  if FSSL <> nil then
    SSL_free(FSSL); // frees both BIOs
  inherited Destroy;
end;

procedure TRALMQTTDTLSSession.Step;
var
  vRes, vError: Integer;
begin
  if FDone then
    Exit;
  vRes := SSL_do_handshake(FSSL);
  if vRes = 1 then
  begin
    FDone := True;
    Exit;
  end;
  vError := SSL_get_error(FSSL, vRes);
  if (vError <> SSL_ERROR_WANT_READ) and (vError <> SSL_ERROR_WANT_WRITE) then
    RaiseTLS('DTLS');
end;

procedure TRALMQTTDTLSSession.Feed(const ADatagram: TBytes; APlain: TList);
var
  vBuf: array[0..65535] of Byte;
  vRes: Integer;
  vData: TRALMQTTDTLSPlain;
begin
  if ADatagram <> nil then
    BIO_write(FRead, @ADatagram[0], Length(ADatagram));
  if not FDone then
  begin
    Step;
    if not FDone then
      Exit;
  end;
  repeat
    vRes := SSL_read(FSSL, @vBuf[0], SizeOf(vBuf));
    if vRes > 0 then
    begin
      vData := TRALMQTTDTLSPlain.Create;
      SetLength(vData.Data, vRes);
      Move(vBuf[0], vData.Data[0], vRes);
      APlain.Add(vData);
    end;
  until vRes <= 0;
end;

function TRALMQTTDTLSSession.PeerCertificate: TRALCertInfo;
begin
  Result := CertificateOf(FSSL);
end;

function TRALMQTTDTLSSession.Seal(const APlain: TBytes): TBytes;
begin
  if (not FDone) or (APlain = nil) then
  begin
    Result := nil;
    Exit;
  end;
  if SSL_write(FSSL, @APlain[0], Length(APlain)) <= 0 then
    RaiseTLS('SSL_write');
  Result := TakeOutput;
end;

function TRALMQTTDTLSSession.TakeOutput: TBytes;
var
  vPending: NativeInt;
begin
  Result := nil;
  vPending := BIO_ctrl(FWrite, BIO_CTRL_PENDING, 0, nil);
  if vPending <= 0 then
    Exit;
  SetLength(Result, vPending);
  BIO_read(FWrite, @Result[0], vPending);
end;

procedure TRALMQTTDTLSSession.Tick;
begin
  if not FDone then
    SSL_ctrl(FSSL, DTLS_CTRL_HANDLE_TIMEOUT, 0, nil);
end;

initialization
  GLock := TCriticalSection.Create;

finalization
  FreeAndNil(GSSL);
  FreeAndNil(GCrypto);
  FreeAndNil(GLock);

end.
