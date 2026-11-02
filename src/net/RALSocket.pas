/// TCP and UDP sockets over the operating system's own socket API, the same
/// code on every system and compiler: only where each function comes from
/// (ws2_32.dll, or the C library) and a handful of constants change, and they
/// are all in the declarations block at the top of the implementation. Waiting
/// is poll() everywhere (WSAPoll on Windows). Also the channel classes the
/// protocols stack on a socket (TLS, WebSocket, a proxy).
/// Package RALNetPackage: used by MQTT, and meant for every protocol RAL speaks
/// on its own sockets (e-mail, a balancer, WebSocket clients).
unit RALSocket;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
  {$PACKRECORDS C}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts;

const
  /// Read or Write found nothing to do without waiting.
  RALSocketWouldBlock = -1;
  /// poll event: data to read (or a connection to accept).
  RALPollIn = $0001;
  /// poll event: room to write (or a connect that finished).
  RALPollOut = $0004;
  /// poll event: an error on the socket.
  RALPollErr = $0008;
  /// poll event: the other side hung up.
  RALPollHup = $0010;

type
  {$IFDEF RALWindows}
  /// A socket handle of the system.
  TRALSocketHandle = NativeUInt;
  {$ELSE}
  /// A socket handle of the system.
  TRALSocketHandle = Integer;
  {$ENDIF}

  /// Which IP family a socket uses.
  TRALIPFamily = (
    /// Whatever the name resolves to; IPv6 and IPv4 tried in turn (RFC 8305).
    mifAny,
    /// IPv4 only.
    mifIPv4,
    /// IPv6 only.
    mifIPv6);

  /// A socket address of any family: the system's sockaddr, as bytes.
  TRALSocketAddress = record
    /// The sockaddr.
    Data: array[0..127] of Byte;
    /// Bytes of Data in use.
    Size: IntegerRAL;
  end;

  /// One entry of RALPoll: a handle, the events asked, the events that came.
  TRALPollItem = record
    /// Socket to watch.
    Handle: TRALSocketHandle;
    /// RALPollIn and/or RALPollOut.
    Events: Word;
    /// Events that happened, after RALPoll.
    Returned: Word;
  end;
  /// Array of poll entries.
  TRALPollItems = array of TRALPollItem;

  /// Raised for a socket failure; Code is the system's error number.
  ERALSocketError = class(Exception)
  private
    FCode: IntegerRAL;
  public
    /// AMessage, with the system error ACode.
    constructor CreateCode(ACode: IntegerRAL; const AMessage: string);
    /// The system's error number (errno, or the WSA code on Windows).
    property Code: IntegerRAL read FCode;
  end;

  { A TCP or UDP socket, always non-blocking underneath; the blocking calls
    (Connect, WaitFor) wait with poll and a deadline. TCP_NODELAY is set on
    every TCP socket: protocols write small packets and Nagle's delay stalls a
    request and its answer by 40 ms or more }
  TRALSocket = class
  private
    FFamily: IntegerRAL;
    FHandle: TRALSocketHandle;
    FPeerAddress: StringRAL;
    FPeerPort: IntegerRAL;

    function GetActive: boolean;
    procedure SetOptions(ATCP: boolean);
  public
    constructor Create;
    /// A socket over a handle a listener accepted.
    constructor CreateAccepted(AHandle: TRALSocketHandle; const AAddress: TRALSocketAddress);
    destructor Destroy; override;

    { Accepts a waiting connection of a listening socket; nil when none is
      waiting. The new socket is non-blocking with the TCP options set }
    function Accept: TRALSocket;
    { Binds to AAddress (empty is every address) and APort, and listens.
      AFamily mifAny opens IPv6 with IPv4 mapped where the system allows it }
    procedure Listen(const AAddress: StringRAL; APort: IntegerRAL;
      AFamily: TRALIPFamily = mifIPv4; ABacklog: IntegerRAL = 128);
    /// Closes the socket; safe to call twice.
    procedure Close;
    { Connects to AHost:APort within ATimeout milliseconds. The name is
      resolved now, every time (an address may change between reconnects),
      and with several addresses the attempts overlap: a new one starts every
      250 ms while the earlier ones still wait, and the first to answer wins
      (RFC 8305, the simple form) }
    procedure Connect(const AHost: StringRAL; APort: IntegerRAL;
      ATimeout: IntegerRAL; AFamily: TRALIPFamily = mifAny);
    /// Opens a UDP socket bound to APort (0: any) on AAddress (empty: every address).
    procedure OpenUDP(const AAddress: StringRAL; APort: IntegerRAL;
      AFamily: TRALIPFamily = mifIPv4);
    { Reads what is there, up to ACount bytes, without waiting: bytes read, 0
      when the other side closed, RALSocketWouldBlock when there is nothing yet.
      Raises on an error }
    function Receive(var ABuffer; ACount: IntegerRAL): IntegerRAL;
    { Reads one datagram into ABuffer (up to ACount bytes) and who sent it;
      RALSocketWouldBlock when none is waiting }
    function ReceiveFrom(var ABuffer; ACount: IntegerRAL; out AFrom: TRALSocketAddress): IntegerRAL;
    { Writes what the system takes now, up to ACount bytes, without waiting:
      bytes taken (maybe fewer), RALSocketWouldBlock when none. Raises on an error }
    function Send(const ABuffer; ACount: IntegerRAL): IntegerRAL;
    /// Sends one datagram to ATo.
    procedure SendTo(const ABuffer; ACount: IntegerRAL; const ATo: TRALSocketAddress);
    /// Ends the sending side (FIN), keeping the socket open to read.
    procedure ShutdownSend;
    /// Lets a UDP socket send to a broadcast address (gateway discovery).
    procedure EnableBroadcast;
    { Waits up to ATimeout milliseconds (-1: no limit) for AEvents
      (RALPollIn, RALPollOut); the events that came, 0 on timeout }
    function WaitFor(AEvents: Word; ATimeout: IntegerRAL): Word;
    /// The port the socket is bound to (a listener on port 0 gets one from the system).
    function LocalPort: IntegerRAL;

    /// The socket is open.
    property Active: boolean read GetActive;
    /// The system handle.
    property Handle: TRALSocketHandle read FHandle;
    /// Numeric address of the other side, after Connect or Accept.
    property PeerAddress: StringRAL read FPeerAddress;
    /// Port of the other side, after Connect or Accept.
    property PeerPort: IntegerRAL read FPeerPort;
  end;

  { A byte stream over a connection, the layer the transports stack:
    the socket itself, TLS over it, WebSocket over either. Read and Write never
    wait; the Wait* helpers wait on the socket with a deadline }
  TRALChannel = class
  private
    FSocket: TRALSocket;
  protected
    function GetSocket: TRALSocket; virtual;
  public
    { Bytes read (up to ACount), 0 when the connection closed, or
      RALSocketWouldBlock when nothing is there yet }
    function Read(var ABuffer; ACount: IntegerRAL): IntegerRAL; virtual; abstract;
    { Bytes taken (up to ACount, maybe 0 when the layer below is full), or
      RALSocketWouldBlock }
    function Write(const ABuffer; ACount: IntegerRAL): IntegerRAL; virtual; abstract;
    /// True when bytes are already buffered in the layer, so Read will not need the socket.
    function HasBuffered: boolean; virtual;
    /// Events the layer needs from the socket to make progress (TLS may need to write to read).
    function WantedEvents: Word; virtual;
    { Writes what a layer queued and did not get out yet; True when nothing
      is left. Never waits }
    function Flush: boolean; virtual;
    /// Closes the layer (a TLS close_notify, a WebSocket close) and the socket.
    procedure Close; virtual;
    { Reads up to ACount bytes, waiting up to ATimeout milliseconds for the
      first of them: bytes read, 0 when closed, RALSocketWouldBlock on timeout }
    function ReadWait(var ABuffer; ACount, ATimeout: IntegerRAL): IntegerRAL;
    { Writes all ACount bytes. AStallTimeout is how long it may go without
      progress, not the whole time: a slow link that keeps moving is never cut.
      Raises when it stalls that long or the connection breaks }
    procedure WriteAll(const ABuffer; ACount, AStallTimeout: IntegerRAL);

    /// The socket at the bottom of the stack.
    property Socket: TRALSocket read GetSocket;
  end;

  /// The socket as a channel: plain TCP.
  TRALSocketChannel = class(TRALChannel)
  private
    FOwnsSocket: boolean;
  public
    /// Channel over ASocket; AOwnsSocket frees it with the channel.
    constructor Create(ASocket: TRALSocket; AOwnsSocket: boolean = True);
    destructor Destroy; override;
    procedure Close; override;
    function Read(var ABuffer; ACount: IntegerRAL): IntegerRAL; override;
    function Write(const ABuffer; ACount: IntegerRAL): IntegerRAL; override;
  end;

  /// A channel over another channel; the base of TLS and WebSocket.
  TRALLayerChannel = class(TRALChannel)
  private
    FInner: TRALChannel;
    FOwnsInner: boolean;
  protected
    function GetSocket: TRALSocket; override;
  public
    /// Layer over AInner; AOwnsInner frees it with this one.
    constructor Create(AInner: TRALChannel; AOwnsInner: boolean = True);
    destructor Destroy; override;
    procedure Close; override;
    /// The channel below.
    property Inner: TRALChannel read FInner;
  end;

{ Waits up to ATimeout milliseconds (-1: no limit) for any of the entries:
  the number with events, 0 on timeout. Interrupted waits are resumed }
function RALPoll(var AItems: TRALPollItems; ACount, ATimeout: IntegerRAL): IntegerRAL;
/// Numeric text of an address ('192.0.2.1', '2001:db8::1').
function RALAddressText(const AAddress: TRALSocketAddress): StringRAL;
/// Port of an address.
function RALAddressPort(const AAddress: TRALSocketAddress): IntegerRAL;
/// True when both are the same address and port.
function RALSameAddress(const A, B: TRALSocketAddress): boolean;
{ Resolves AHost:APort into AAddress for a datagram socket of AFamily; False
  when the name does not resolve }
function RALResolve(const AHost: StringRAL; APort: IntegerRAL;
  AFamily: TRALIPFamily; out AAddress: TRALSocketAddress): boolean;
/// Text of a system socket error.
function RALSocketErrorText(ACode: IntegerRAL): string;
{ Milliseconds of a clock that only goes forward, unaffected by changes of the
  system time: the deadlines of the sockets and of the protocols above them }
function RALSocketNow: Int64RAL;

implementation

{$IFNDEF FPC}
  {$IFDEF RALWindows}
uses
  Windows;
  {$ELSE}
uses
  Posix.Base, Posix.Time;
  {$ENDIF}
{$ENDIF}

{ ---------------------------------------------------------------------------
  Declarations: the only part that differs between systems. Everything after
  this block is the same code everywhere.
  --------------------------------------------------------------------------- }

const
  cSOCK_STREAM = 1;
  cSOCK_DGRAM = 2;
  cIPPROTO_TCP = 6;
  cIPPROTO_UDP = 17;
  cTCP_NODELAY = 1;
  cAI_PASSIVE = 1;

{$IFDEF RALWindows}
const
  cLib = 'ws2_32.dll';
  cAF_INET = 2;
  cAF_INET6 = 23;
  cSOL_SOCKET = $FFFF;
  cSO_REUSEADDR = $0004;
  cSO_KEEPALIVE = $0008;
  cSO_BROADCAST = $0020;
  cSO_ERROR = $1007;
  cIPPROTO_IPV6 = 41;
  cIPV6_V6ONLY = 27;
  cNI_NUMERICHOST = 2;
  cNI_NUMERICSERV = 8;
  cEAGAIN = 10035;      // WSAEWOULDBLOCK
  cEINPROGRESS = 10036;
  cEINTR = 10004;
  cFIONBIO = Integer($8004667E);
  cSHUT_WR = 1;
  cInvalid = TRALSocketHandle(not TRALSocketHandle(0));
  cSendFlags = 0;

type
  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
    ai_flags: Integer;
    ai_family: Integer;
    ai_socktype: Integer;
    ai_protocol: Integer;
    ai_addrlen: NativeUInt;
    ai_canonname: PAnsiChar;
    ai_addr: Pointer;
    ai_next: PAddrInfo;
  end;
  TSockLen = Integer;
  TPollFd = record
    fd: TRALSocketHandle;
    events: SmallInt;
    revents: SmallInt;
  end;

function sys_socket(af, typ, protocol: Integer): TRALSocketHandle; stdcall; external cLib name 'socket';
function sys_connect(s: TRALSocketHandle; name: Pointer; namelen: TSockLen): Integer; stdcall; external cLib name 'connect';
function sys_bind(s: TRALSocketHandle; name: Pointer; namelen: TSockLen): Integer; stdcall; external cLib name 'bind';
function sys_listen(s: TRALSocketHandle; backlog: Integer): Integer; stdcall; external cLib name 'listen';
function sys_accept(s: TRALSocketHandle; addr: Pointer; var addrlen: TSockLen): TRALSocketHandle; stdcall; external cLib name 'accept';
function sys_send(s: TRALSocketHandle; const buf; len, flags: Integer): Integer; stdcall; external cLib name 'send';
function sys_recv(s: TRALSocketHandle; var buf; len, flags: Integer): Integer; stdcall; external cLib name 'recv';
function sys_sendto(s: TRALSocketHandle; const buf; len, flags: Integer; addrto: Pointer; tolen: TSockLen): Integer; stdcall; external cLib name 'sendto';
function sys_recvfrom(s: TRALSocketHandle; var buf; len, flags: Integer; from: Pointer; var fromlen: TSockLen): Integer; stdcall; external cLib name 'recvfrom';
function sys_closesocket(s: TRALSocketHandle): Integer; stdcall; external cLib name 'closesocket';
function sys_shutdown(s: TRALSocketHandle; how: Integer): Integer; stdcall; external cLib name 'shutdown';
function sys_setsockopt(s: TRALSocketHandle; level, optname: Integer; optval: Pointer; optlen: TSockLen): Integer; stdcall; external cLib name 'setsockopt';
function sys_getsockopt(s: TRALSocketHandle; level, optname: Integer; optval: Pointer; var optlen: TSockLen): Integer; stdcall; external cLib name 'getsockopt';
function sys_getsockname(s: TRALSocketHandle; name: Pointer; var namelen: TSockLen): Integer; stdcall; external cLib name 'getsockname';
function sys_getpeername(s: TRALSocketHandle; name: Pointer; var namelen: TSockLen): Integer; stdcall; external cLib name 'getpeername';
function sys_ioctlsocket(s: TRALSocketHandle; cmd: Integer; var arg: Cardinal): Integer; stdcall; external cLib name 'ioctlsocket';
function sys_poll(fds: Pointer; nfds: Cardinal; timeout: Integer): Integer; stdcall; external cLib name 'WSAPoll';
function sys_getaddrinfo(node, service: PAnsiChar; hints: PAddrInfo; var res: PAddrInfo): Integer; stdcall; external cLib name 'getaddrinfo';
procedure sys_freeaddrinfo(ai: PAddrInfo); stdcall; external cLib name 'freeaddrinfo';
function sys_getnameinfo(sa: Pointer; salen: TSockLen; host: PAnsiChar; hostlen: Cardinal;
  serv: PAnsiChar; servlen: Cardinal; flags: Integer): Integer; stdcall; external cLib name 'getnameinfo';
function WSAStartup(AVersion: Word; AData: Pointer): Integer; stdcall; external cLib name 'WSAStartup';
function WSACleanup: Integer; stdcall; external cLib name 'WSACleanup';
function WSAGetLastError: Integer; stdcall; external cLib name 'WSAGetLastError';

function LastError: IntegerRAL;
begin
  Result := WSAGetLastError;
end;

{ WSAPoll has its own bit values: POLLIN is 0x0300, POLLOUT 0x0010, POLLERR
  0x0001, POLLHUP 0x0002; asking for an event it does not take is an error }
function PollEventsToSys(AEvents: Word): SmallInt;
begin
  Result := 0;
  if (AEvents and RALPollIn) <> 0 then
    Result := Result or $0300;
  if (AEvents and RALPollOut) <> 0 then
    Result := Result or $0010;
end;

function PollEventsFromSys(AEvents: SmallInt): Word;
begin
  Result := 0;
  if (AEvents and $0300) <> 0 then
    Result := Result or RALPollIn;
  if (AEvents and $0010) <> 0 then
    Result := Result or RALPollOut;
  if (AEvents and $0005) <> 0 then // POLLERR, POLLNVAL
    Result := Result or RALPollErr;
  if (AEvents and $0002) <> 0 then
    Result := Result or RALPollHup;
end;

function SetNonBlocking(AHandle: TRALSocketHandle): boolean;
var
  vArg: Cardinal;
begin
  vArg := 1;
  Result := sys_ioctlsocket(AHandle, cFIONBIO, vArg) = 0;
end;
{$ELSE}
const
  cAF_INET = 2;
  {$IF Defined(DARWIN) or Defined(MACOS) or Defined(IOS) or Defined(FREEBSD) or Defined(BSD)}
    {$DEFINE RALSOCKET_BSD}
  {$IFEND}
  {$IFDEF RALSOCKET_BSD}
  cAF_INET6 = 28 + {$IFDEF FREEBSD}0{$ELSE}2{$ENDIF}; // 30 on Apple, 28 on FreeBSD
  cSOL_SOCKET = $FFFF;
  cSO_REUSEADDR = $0004;
  cSO_KEEPALIVE = $0008;
  cSO_BROADCAST = $0020;
  cSO_ERROR = $1007;
  cSO_NOSIGPIPE = $1022;
  cIPPROTO_IPV6 = 41;
  cIPV6_V6ONLY = 27;
  cNI_NUMERICHOST = 2;
  cNI_NUMERICSERV = 8;
  cEAGAIN = 35;
  cEINPROGRESS = 36;
  cEINTR = 4;
  cO_NONBLOCK = $0004;
  {$IFDEF FREEBSD}
  cSendFlags = $20000; // MSG_NOSIGNAL
  {$ELSE}
  cSendFlags = 0;      // Apple: SO_NOSIGPIPE on the socket instead
  {$ENDIF}
  {$ELSE}
  cAF_INET6 = 10;
  cSOL_SOCKET = 1;
  cSO_REUSEADDR = 2;
  cSO_KEEPALIVE = 9;
  cSO_BROADCAST = 6;
  cSO_ERROR = 4;
  cIPPROTO_IPV6 = 41;
  cIPV6_V6ONLY = 26;
  cTCP_KEEPIDLE = 4;
  cTCP_KEEPINTVL = 5;
  cTCP_KEEPCNT = 6;
  cNI_NUMERICHOST = 1;
  cNI_NUMERICSERV = 2;
  cEAGAIN = 11;
  cEINPROGRESS = 115;
  cEINTR = 4;
  cO_NONBLOCK = $0800;
  cSendFlags = $4000;  // MSG_NOSIGNAL: a write to a closed peer must not kill the process
  {$ENDIF}
  cF_GETFL = 3;
  cF_SETFL = 4;
  cSHUT_WR = 1;
  cInvalid = -1;
  {$IFDEF FPC}
  cLib = 'c';
  {$ELSE}
  cLib = libc;
  {$ENDIF}

type
  PAddrInfo = ^TAddrInfo;
  {$IFDEF RALSOCKET_BSD}
  TAddrInfo = record
    ai_flags: Integer;
    ai_family: Integer;
    ai_socktype: Integer;
    ai_protocol: Integer;
    ai_addrlen: Cardinal;
    ai_canonname: PAnsiChar;
    ai_addr: Pointer;
    ai_next: PAddrInfo;
  end;
  {$ELSE}
  TAddrInfo = record
    ai_flags: Integer;
    ai_family: Integer;
    ai_socktype: Integer;
    ai_protocol: Integer;
    ai_addrlen: Cardinal;
    ai_addr: Pointer;
    ai_canonname: PAnsiChar;
    ai_next: PAddrInfo;
  end;
  {$ENDIF}
  TSockLen = Cardinal;
  TPollFd = record
    fd: Integer;
    events: SmallInt;
    revents: SmallInt;
  end;

function sys_socket(af, typ, protocol: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'socket';
function sys_connect(s: Integer; name: Pointer; namelen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'connect';
function sys_bind(s: Integer; name: Pointer; namelen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'bind';
function sys_listen(s: Integer; backlog: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'listen';
function sys_accept(s: Integer; addr: Pointer; var addrlen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'accept';
function sys_send(s: Integer; const buf; len: NativeUInt; flags: Integer): NativeInt; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'send';
function sys_recv(s: Integer; var buf; len: NativeUInt; flags: Integer): NativeInt; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'recv';
function sys_sendto(s: Integer; const buf; len: NativeUInt; flags: Integer; addrto: Pointer; tolen: TSockLen): NativeInt; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'sendto';
function sys_recvfrom(s: Integer; var buf; len: NativeUInt; flags: Integer; from: Pointer; var fromlen: TSockLen): NativeInt; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'recvfrom';
function sys_close(s: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'close';
function sys_shutdown(s: Integer; how: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'shutdown';
function sys_setsockopt(s: Integer; level, optname: Integer; optval: Pointer; optlen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'setsockopt';
function sys_getsockopt(s: Integer; level, optname: Integer; optval: Pointer; var optlen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'getsockopt';
function sys_getsockname(s: Integer; name: Pointer; var namelen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'getsockname';
function sys_getpeername(s: Integer; name: Pointer; var namelen: TSockLen): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'getpeername';
function sys_fcntl(fd, cmd, arg: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'fcntl';
function sys_poll(fds: Pointer; nfds: Cardinal; timeout: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'poll';
function sys_getaddrinfo(node, service: PAnsiChar; hints: PAddrInfo; var res: PAddrInfo): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'getaddrinfo';
procedure sys_freeaddrinfo(ai: PAddrInfo); cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'freeaddrinfo';
function sys_getnameinfo(sa: Pointer; salen: TSockLen; host: PAnsiChar; hostlen: Cardinal;
  serv: PAnsiChar; servlen: Cardinal; flags: Integer): Integer; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'getnameinfo';
{$IFDEF RALSOCKET_BSD}
function sys_errno: PInteger; cdecl; external cLib name {$IFNDEF FPC}_PU + {$ENDIF}'__error';
{$ELSE}
  {$IF Defined(ANDROID) or Defined(Android32) or Defined(Android64)}
function sys_errno: PInteger; cdecl; external cLib name '__errno';
  {$ELSE}
function sys_errno: PInteger; cdecl; external cLib name '__errno_location';
  {$IFEND}
{$ENDIF}

function LastError: IntegerRAL;
begin
  Result := sys_errno^;
end;

{ POSIX poll: the public constants are its own values }
function PollEventsToSys(AEvents: Word): SmallInt;
begin
  Result := SmallInt(AEvents and (RALPollIn or RALPollOut));
end;

function PollEventsFromSys(AEvents: SmallInt): Word;
begin
  Result := Word(AEvents) and (RALPollIn or RALPollOut or RALPollErr or
    RALPollHup or $0020); // $0020 POLLNVAL
  if (Result and $0020) <> 0 then
    Result := (Result and not Word($0020)) or RALPollErr;
end;

function sys_closesocket(s: Integer): Integer;
begin
  Result := sys_close(s);
end;

function SetNonBlocking(AHandle: TRALSocketHandle): boolean;
var
  vFlags: Integer;
begin
  vFlags := sys_fcntl(AHandle, cF_GETFL, 0);
  Result := (vFlags <> -1) and (sys_fcntl(AHandle, cF_SETFL, vFlags or cO_NONBLOCK) <> -1);
end;
{$ENDIF}

{ ---------------------------------------------------------------------------
  From here on, the same code on every system.
  --------------------------------------------------------------------------- }

function FamilyCode(AFamily: TRALIPFamily): Integer;
begin
  case AFamily of
    mifIPv4: Result := cAF_INET;
    mifIPv6: Result := cAF_INET6;
  else
    Result := 0; // AF_UNSPEC
  end;
end;

function RALSocketErrorText(ACode: IntegerRAL): string;
begin
  Result := SysErrorMessage(ACode);
  if Result = '' then
    Result := Format(emSocketErrorCode, [ACode]);
end;

procedure RaiseSocket(const AWhat: string; ACode: IntegerRAL);
begin
  raise ERALSocketError.CreateCode(ACode, Format(emSocketError,
    [AWhat, RALSocketErrorText(ACode), ACode]));
end;

{ the family field: a word on Windows and Linux, a byte after sa_len on BSD }
function AddressFamily(const AAddress: TRALSocketAddress): Integer;
begin
  {$IFDEF RALSOCKET_BSD}
  Result := AAddress.Data[1];
  {$ELSE}
  Result := PWord(@AAddress.Data[0])^;
  {$ENDIF}
end;

function RALAddressPort(const AAddress: TRALSocketAddress): IntegerRAL;
begin
  // the port sits at offset 2, in network order, in both sockaddr_in and _in6
  Result := (AAddress.Data[2] shl 8) or AAddress.Data[3];
end;

function RALAddressText(const AAddress: TRALSocketAddress): StringRAL;
var
  vHost: array[0..63] of AnsiChar;
begin
  Result := '';
  if AAddress.Size <= 0 then
    Exit;
  FillChar(vHost, SizeOf(vHost), 0);
  if sys_getnameinfo(@AAddress.Data[0], AAddress.Size, @vHost[0], SizeOf(vHost),
       nil, 0, cNI_NUMERICHOST) = 0 then
    Result := StringRAL(PAnsiChar(@vHost[0]));
  // an IPv4 client on a dual-stack socket arrives as ::ffff:a.b.c.d
  if Copy(Result, 1, 7) = '::ffff:' then
    Delete(Result, 1, 7);
end;

function RALSameAddress(const A, B: TRALSocketAddress): boolean;
begin
  Result := (A.Size = B.Size) and (A.Size > 0) and CompareMem(@A.Data[0], @B.Data[0], A.Size);
end;

function Resolve(const AHost: StringRAL; APort: IntegerRAL; AFamily: TRALIPFamily;
  ASockType: Integer; APassive: boolean; out AList: PAddrInfo): Integer;
var
  vHints: TAddrInfo;
  vHost: AnsiString;
  vPort: AnsiString;
  vNode: PAnsiChar;
begin
  FillChar(vHints, SizeOf(vHints), 0);
  vHints.ai_family := FamilyCode(AFamily);
  vHints.ai_socktype := ASockType;
  if APassive then
    vHints.ai_flags := cAI_PASSIVE;
  vHost := AnsiString(AHost);
  // brackets of a URL ('[::1]') are not part of the address
  if (Length(vHost) > 1) and (vHost[1] = '[') and (vHost[Length(vHost)] = ']') then
    vHost := Copy(vHost, 2, Length(vHost) - 2);
  if vHost = '' then
    vNode := nil
  else
    vNode := PAnsiChar(vHost);
  vPort := AnsiString(IntToStr(APort));
  AList := nil;
  Result := sys_getaddrinfo(vNode, PAnsiChar(vPort), @vHints, AList);
end;

function RALResolve(const AHost: StringRAL; APort: IntegerRAL;
  AFamily: TRALIPFamily; out AAddress: TRALSocketAddress): boolean;
var
  vList: PAddrInfo;
begin
  FillChar(AAddress, SizeOf(AAddress), 0);
  Result := (Resolve(AHost, APort, AFamily, cSOCK_DGRAM, False, vList) = 0) and (vList <> nil);
  if not Result then
    Exit;
  try
    AAddress.Size := vList^.ai_addrlen;
    if AAddress.Size > SizeOf(AAddress.Data) then
      AAddress.Size := SizeOf(AAddress.Data);
    Move(vList^.ai_addr^, AAddress.Data[0], AAddress.Size);
  finally
    sys_freeaddrinfo(vList);
  end;
end;

function RALPoll(var AItems: TRALPollItems; ACount, ATimeout: IntegerRAL): IntegerRAL;
var
  vFds: array of TPollFd;
  vInt: IntegerRAL;
  vDeadline, vNow: Int64RAL;
  vWait: IntegerRAL;
begin
  Result := 0;
  if ACount <= 0 then
  begin
    if ATimeout > 0 then
      Sleep(ATimeout);
    Exit;
  end;
  SetLength(vFds, ACount);
  for vInt := 0 to ACount - 1 do
  begin
    vFds[vInt].fd := AItems[vInt].Handle;
    vFds[vInt].events := PollEventsToSys(AItems[vInt].Events);
    vFds[vInt].revents := 0;
    AItems[vInt].Returned := 0;
  end;
  if ATimeout >= 0 then
    vDeadline := RALSocketNow + ATimeout
  else
    vDeadline := 0;
  vWait := ATimeout;
  repeat
    Result := sys_poll(@vFds[0], ACount, vWait);
    if (Result >= 0) or (LastError <> cEINTR) then
      Break;
    // a signal woke the wait: wait again for what is left
    if ATimeout >= 0 then
    begin
      vNow := RALSocketNow;
      if vNow >= vDeadline then
      begin
        Result := 0;
        Break;
      end;
      vWait := vDeadline - vNow;
    end;
  until False;
  if Result < 0 then
    RaiseSocket('poll', LastError);
  for vInt := 0 to ACount - 1 do
    AItems[vInt].Returned := PollEventsFromSys(vFds[vInt].revents);
end;

{$IF Defined(RALWindows) and not Defined(FPC)}
function GetTickCount64: UInt64; stdcall; external kernel32 name 'GetTickCount64';
{$IFEND}

function RALSocketNow: Int64RAL;
{$IF Defined(POSIX) and not Defined(FPC)}
var
  vTime: timespec;
{$IFEND}
begin
  {$IFDEF FPC}
  Result := Int64RAL(GetTickCount64);
  {$ELSE}
    {$IFDEF RALWindows}
    Result := Int64RAL(GetTickCount64);
    {$ELSE}
    clock_gettime(CLOCK_MONOTONIC, @vTime);
    Result := Int64RAL(vTime.tv_sec) * 1000 + vTime.tv_nsec div 1000000;
    {$ENDIF}
  {$ENDIF}
end;

{ ERALSocketError }

constructor ERALSocketError.CreateCode(ACode: IntegerRAL; const AMessage: string);
begin
  inherited Create(AMessage);
  FCode := ACode;
end;

{ TRALSocket }

constructor TRALSocket.Create;
begin
  inherited Create;
  FHandle := cInvalid;
end;

constructor TRALSocket.CreateAccepted(AHandle: TRALSocketHandle;
  const AAddress: TRALSocketAddress);
begin
  Create;
  FHandle := AHandle;
  FFamily := AddressFamily(AAddress);
  FPeerAddress := RALAddressText(AAddress);
  FPeerPort := RALAddressPort(AAddress);
  SetNonBlocking(FHandle);
  SetOptions(True);
end;

destructor TRALSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TRALSocket.GetActive: boolean;
begin
  Result := FHandle <> cInvalid;
end;

procedure TRALSocket.SetOptions(ATCP: boolean);
var
  vOne: Integer;
begin
  vOne := 1;
  {$IFDEF RALSOCKET_BSD}
  {$IFNDEF FREEBSD}
  sys_setsockopt(FHandle, cSOL_SOCKET, cSO_NOSIGPIPE, @vOne, SizeOf(vOne));
  {$ENDIF}
  {$ENDIF}
  if not ATCP then
    Exit;
  sys_setsockopt(FHandle, cIPPROTO_TCP, cTCP_NODELAY, @vOne, SizeOf(vOne));
  { the TCP keep alive finds a dead path even when the protocol is quiet; where the
    system lets the timing be set, a minute idle and three probes ten seconds
    apart, instead of the two hours most systems start with }
  sys_setsockopt(FHandle, cSOL_SOCKET, cSO_KEEPALIVE, @vOne, SizeOf(vOne));
  {$IF not Defined(RALWindows) and not Defined(RALSOCKET_BSD)}
  vOne := 60;
  sys_setsockopt(FHandle, cIPPROTO_TCP, cTCP_KEEPIDLE, @vOne, SizeOf(vOne));
  vOne := 10;
  sys_setsockopt(FHandle, cIPPROTO_TCP, cTCP_KEEPINTVL, @vOne, SizeOf(vOne));
  vOne := 3;
  sys_setsockopt(FHandle, cIPPROTO_TCP, cTCP_KEEPCNT, @vOne, SizeOf(vOne));
  {$IFEND}
end;

function TRALSocket.Accept: TRALSocket;
var
  vAddress: TRALSocketAddress;
  vLen: TSockLen;
  vHandle: TRALSocketHandle;
  vError: IntegerRAL;
begin
  Result := nil;
  FillChar(vAddress, SizeOf(vAddress), 0);
  vLen := SizeOf(vAddress.Data);
  vHandle := sys_accept(FHandle, @vAddress.Data[0], vLen);
  if vHandle = cInvalid then
  begin
    vError := LastError;
    if (vError = cEAGAIN) or (vError = cEINTR) then
      Exit;
    RaiseSocket('accept', vError);
  end;
  vAddress.Size := vLen;
  Result := TRALSocket.CreateAccepted(vHandle, vAddress);
end;

procedure TRALSocket.Close;
begin
  if FHandle = cInvalid then
    Exit;
  sys_closesocket(FHandle);
  FHandle := cInvalid;
end;

procedure TRALSocket.Connect(const AHost: StringRAL; APort: IntegerRAL;
  ATimeout: IntegerRAL; AFamily: TRALIPFamily);
const
  cAttemptDelay = 250; // RFC 8305 5: the Connection Attempt Delay
type
  TAttempt = record
    Handle: TRALSocketHandle;
    Address: TRALSocketAddress;
  end;
var
  vList, vItem: PAddrInfo;
  vAddresses: array of TRALSocketAddress;
  vFamilies: array of Integer;
  vOrdered: array of IntegerRAL;
  vAttempts: array of TAttempt;
  vPoll: TRALPollItems;
  vCount, vInt, vNext, vV6, vV4, vRes, vError, vLastError: IntegerRAL;
  vDeadline, vNextStart, vNow: Int64RAL;
  vHandle: TRALSocketHandle;
  vWinner: IntegerRAL;
  vDone: boolean;

  procedure DropAttempt(AIndex: IntegerRAL);
  begin
    sys_closesocket(vAttempts[AIndex].Handle);
    vAttempts[AIndex] := vAttempts[High(vAttempts)];
    SetLength(vAttempts, Length(vAttempts) - 1);
  end;

  function PendingError(AHandle: TRALSocketHandle): IntegerRAL;
  var
    vValue: Integer;
    vSize: TSockLen;
  begin
    vValue := 0;
    vSize := SizeOf(vValue);
    if sys_getsockopt(AHandle, cSOL_SOCKET, cSO_ERROR, @vValue, vSize) <> 0 then
      Result := LastError
    else
      Result := vValue;
  end;

begin
  Close;
  vError := Resolve(AHost, APort, AFamily, cSOCK_STREAM, False, vList);
  if (vError <> 0) or (vList = nil) then
    raise ERALSocketError.CreateCode(vError, Format(emSocketResolveFailed, [string(AHost)]));
  try
    vCount := 0;
    vItem := vList;
    while vItem <> nil do
    begin
      if (vItem^.ai_family = cAF_INET) or (vItem^.ai_family = cAF_INET6) then
      begin
        SetLength(vAddresses, vCount + 1);
        SetLength(vFamilies, vCount + 1);
        FillChar(vAddresses[vCount], SizeOf(TRALSocketAddress), 0);
        vAddresses[vCount].Size := vItem^.ai_addrlen;
        if vAddresses[vCount].Size > SizeOf(vAddresses[vCount].Data) then
          vAddresses[vCount].Size := SizeOf(vAddresses[vCount].Data);
        Move(vItem^.ai_addr^, vAddresses[vCount].Data[0], vAddresses[vCount].Size);
        vFamilies[vCount] := vItem^.ai_family;
        Inc(vCount);
      end;
      vItem := vItem^.ai_next;
    end;
  finally
    sys_freeaddrinfo(vList);
  end;
  if vCount = 0 then
    raise ERALSocketError.CreateCode(0, Format(emSocketResolveFailed, [string(AHost)]));

  { RFC 8305 4: the families take turns, in the order the system gave the
    first of each (its own preference, RFC 6724) }
  SetLength(vOrdered, vCount);
  vV6 := 0;
  vV4 := 0;
  vNext := 0;
  while vNext < vCount do
  begin
    while (vV6 < vCount) and (vFamilies[vV6] <> vFamilies[0]) do
      Inc(vV6);
    if vV6 < vCount then
    begin
      vOrdered[vNext] := vV6;
      Inc(vNext);
      Inc(vV6);
    end;
    while (vV4 < vCount) and (vFamilies[vV4] = vFamilies[0]) do
      Inc(vV4);
    if (vNext < vCount) and (vV4 < vCount) then
    begin
      vOrdered[vNext] := vV4;
      Inc(vNext);
      Inc(vV4);
    end;
  end;

  vDeadline := RALSocketNow + ATimeout;
  vNext := 0;
  vNextStart := 0;
  vWinner := -1;
  vLastError := 0;
  vDone := False;
  SetLength(vAttempts, 0);
  try
    repeat
      vNow := RALSocketNow;
      // start the next attempt when its turn comes, or when nothing is pending
      if (vNext < vCount) and ((vNow >= vNextStart) or (Length(vAttempts) = 0)) then
      begin
        vInt := vOrdered[vNext];
        Inc(vNext);
        vHandle := sys_socket(vFamilies[vInt], cSOCK_STREAM, cIPPROTO_TCP);
        if vHandle <> cInvalid then
        begin
          SetNonBlocking(vHandle);
          vRes := sys_connect(vHandle, @vAddresses[vInt].Data[0], vAddresses[vInt].Size);
          vError := 0;
          if vRes <> 0 then
            vError := LastError;
          if (vRes = 0) or (vError = cEINPROGRESS) or (vError = cEAGAIN) then
          begin
            SetLength(vAttempts, Length(vAttempts) + 1);
            vAttempts[High(vAttempts)].Handle := vHandle;
            vAttempts[High(vAttempts)].Address := vAddresses[vInt];
          end
          else
          begin
            vLastError := vError;
            sys_closesocket(vHandle);
          end;
        end
        else
          vLastError := LastError;
        vNextStart := RALSocketNow + cAttemptDelay;
        Continue;
      end;
      if Length(vAttempts) = 0 then
        Break; // every address failed

      vNow := RALSocketNow;
      if vNow >= vDeadline then
        Break;
      vRes := vDeadline - vNow;
      if (vNext < vCount) and (vNextStart - vNow < vRes) then
        vRes := vNextStart - vNow;
      { Windows before 10 2004 does not flag a refused connect in WSAPoll:
        never wait long, and read SO_ERROR of every attempt after each wait }
      if vRes > 100 then
        vRes := 100;
      if vRes < 1 then
        vRes := 1;
      SetLength(vPoll, Length(vAttempts));
      for vInt := 0 to High(vAttempts) do
      begin
        vPoll[vInt].Handle := vAttempts[vInt].Handle;
        vPoll[vInt].Events := RALPollOut;
      end;
      RALPoll(vPoll, Length(vPoll), vRes);
      vInt := 0;
      vDone := False;
      while vInt <= High(vAttempts) do
      begin
        vError := PendingError(vAttempts[vInt].Handle);
        if vError <> 0 then
        begin
          vLastError := vError;
          DropAttempt(vInt);
          SetLength(vPoll, 0);
          Continue;
        end;
        if (vInt < Length(vPoll)) and ((vPoll[vInt].Returned and RALPollOut) <> 0) then
        begin
          vWinner := vInt;
          vDone := True;
          Break;
        end;
        Inc(vInt);
      end;
    until vDone;

    if vWinner < 0 then
    begin
      if RALSocketNow >= vDeadline then
        raise ERALSocketError.CreateCode(0, Format(emSocketConnectTimeout,
          [string(AHost), APort, ATimeout]));
      RaiseSocket(Format(emSocketConnectTo, [string(AHost), APort]), vLastError);
    end;

    FHandle := vAttempts[vWinner].Handle;
    FFamily := AddressFamily(vAttempts[vWinner].Address);
    FPeerAddress := RALAddressText(vAttempts[vWinner].Address);
    FPeerPort := APort;
    vAttempts[vWinner] := vAttempts[High(vAttempts)];
    SetLength(vAttempts, Length(vAttempts) - 1);
    SetOptions(True);
  finally
    for vInt := 0 to High(vAttempts) do
      sys_closesocket(vAttempts[vInt].Handle);
  end;
end;

procedure TRALSocket.Listen(const AAddress: StringRAL; APort: IntegerRAL;
  AFamily: TRALIPFamily; ABacklog: IntegerRAL);
var
  vList: PAddrInfo;
  vError: Integer;
  vOne, vZero: Integer;
  vFamily: TRALIPFamily;
begin
  Close;
  vFamily := AFamily;
  if vFamily = mifAny then
    vFamily := mifIPv6;
  vError := Resolve(AAddress, APort, vFamily, cSOCK_STREAM, True, vList);
  if (vError <> 0) or (vList = nil) then
    raise ERALSocketError.CreateCode(vError, Format(emSocketResolveFailed, [string(AAddress)]));
  try
    FHandle := sys_socket(vList^.ai_family, cSOCK_STREAM, cIPPROTO_TCP);
    if FHandle = cInvalid then
      RaiseSocket('socket', LastError);
    FFamily := vList^.ai_family;
    vOne := 1;
    { Windows: SO_REUSEADDR would let another process take the port; its
      default already allows a restart while old connections close }
    {$IFNDEF RALWindows}
    sys_setsockopt(FHandle, cSOL_SOCKET, cSO_REUSEADDR, @vOne, SizeOf(vOne));
    {$ENDIF}
    if (AFamily = mifAny) and (FFamily = cAF_INET6) then
    begin
      // one socket for both families: IPv4 clients come as mapped addresses
      vZero := 0;
      sys_setsockopt(FHandle, cIPPROTO_IPV6, cIPV6_V6ONLY, @vZero, SizeOf(vZero));
    end;
    if sys_bind(FHandle, vList^.ai_addr, vList^.ai_addrlen) <> 0 then
    begin
      vError := LastError;
      Close;
      RaiseSocket(Format(emSocketBindTo, [string(AAddress), APort]), vError);
    end;
  finally
    sys_freeaddrinfo(vList);
  end;
  if sys_listen(FHandle, ABacklog) <> 0 then
  begin
    vError := LastError;
    Close;
    RaiseSocket('listen', vError);
  end;
  SetNonBlocking(FHandle);
end;

function TRALSocket.LocalPort: IntegerRAL;
var
  vAddress: TRALSocketAddress;
  vLen: TSockLen;
begin
  Result := 0;
  FillChar(vAddress, SizeOf(vAddress), 0);
  vLen := SizeOf(vAddress.Data);
  if sys_getsockname(FHandle, @vAddress.Data[0], vLen) = 0 then
    Result := RALAddressPort(vAddress);
end;

procedure TRALSocket.OpenUDP(const AAddress: StringRAL; APort: IntegerRAL;
  AFamily: TRALIPFamily);
var
  vList: PAddrInfo;
  vError: Integer;
  vZero: Integer;
  vFamily: TRALIPFamily;
begin
  Close;
  vFamily := AFamily;
  if vFamily = mifAny then
    vFamily := mifIPv6;
  vError := Resolve(AAddress, APort, vFamily, cSOCK_DGRAM, True, vList);
  if (vError <> 0) or (vList = nil) then
    raise ERALSocketError.CreateCode(vError, Format(emSocketResolveFailed, [string(AAddress)]));
  try
    FHandle := sys_socket(vList^.ai_family, cSOCK_DGRAM, cIPPROTO_UDP);
    if FHandle = cInvalid then
      RaiseSocket('socket', LastError);
    FFamily := vList^.ai_family;
    if (AFamily = mifAny) and (FFamily = cAF_INET6) then
    begin
      vZero := 0;
      sys_setsockopt(FHandle, cIPPROTO_IPV6, cIPV6_V6ONLY, @vZero, SizeOf(vZero));
    end;
    if sys_bind(FHandle, vList^.ai_addr, vList^.ai_addrlen) <> 0 then
    begin
      vError := LastError;
      Close;
      RaiseSocket(Format(emSocketBindTo, [string(AAddress), APort]), vError);
    end;
  finally
    sys_freeaddrinfo(vList);
  end;
  SetNonBlocking(FHandle);
  SetOptions(False);
end;

function TRALSocket.Receive(var ABuffer; ACount: IntegerRAL): IntegerRAL;
var
  vError: IntegerRAL;
begin
  if FHandle = cInvalid then
  begin
    Result := 0;
    Exit;
  end;
  Result := sys_recv(FHandle, ABuffer, ACount, 0);
  if Result < 0 then
  begin
    vError := LastError;
    if (vError = cEAGAIN) or (vError = cEINTR) then
      Result := RALSocketWouldBlock
    else
      RaiseSocket('recv', vError);
  end;
end;

function TRALSocket.ReceiveFrom(var ABuffer; ACount: IntegerRAL;
  out AFrom: TRALSocketAddress): IntegerRAL;
var
  vLen: TSockLen;
  vError: IntegerRAL;
begin
  FillChar(AFrom, SizeOf(AFrom), 0);
  vLen := SizeOf(AFrom.Data);
  Result := sys_recvfrom(FHandle, ABuffer, ACount, 0, @AFrom.Data[0], vLen);
  if Result < 0 then
  begin
    vError := LastError;
    if (vError = cEAGAIN) or (vError = cEINTR) then
      Result := RALSocketWouldBlock
    else
      RaiseSocket('recvfrom', vError);
  end
  else
    AFrom.Size := vLen;
end;

function TRALSocket.Send(const ABuffer; ACount: IntegerRAL): IntegerRAL;
var
  vError: IntegerRAL;
begin
  if FHandle = cInvalid then
    RaiseSocket('send', 0);
  Result := sys_send(FHandle, ABuffer, ACount, cSendFlags);
  if Result < 0 then
  begin
    vError := LastError;
    if (vError = cEAGAIN) or (vError = cEINTR) then
      Result := RALSocketWouldBlock
    else
      RaiseSocket('send', vError);
  end;
end;

procedure TRALSocket.SendTo(const ABuffer; ACount: IntegerRAL;
  const ATo: TRALSocketAddress);
var
  vRes: IntegerRAL;
  vError: IntegerRAL;
begin
  vRes := sys_sendto(FHandle, ABuffer, ACount, cSendFlags, @ATo.Data[0], ATo.Size);
  if vRes < 0 then
  begin
    vError := LastError;
    // a full buffer drops the datagram, as the network itself might
    if (vError <> cEAGAIN) and (vError <> cEINTR) then
      RaiseSocket('sendto', vError);
  end;
end;

procedure TRALSocket.EnableBroadcast;
var
  vOne: Integer;
begin
  vOne := 1;
  if FHandle <> cInvalid then
    sys_setsockopt(FHandle, cSOL_SOCKET, cSO_BROADCAST, @vOne, SizeOf(vOne));
end;

procedure TRALSocket.ShutdownSend;
begin
  if FHandle <> cInvalid then
    sys_shutdown(FHandle, cSHUT_WR);
end;

function TRALSocket.WaitFor(AEvents: Word; ATimeout: IntegerRAL): Word;
var
  vPoll: TRALPollItems;
begin
  Result := 0;
  if FHandle = cInvalid then
    Exit;
  SetLength(vPoll, 1);
  vPoll[0].Handle := FHandle;
  vPoll[0].Events := AEvents;
  if RALPoll(vPoll, 1, ATimeout) > 0 then
    Result := vPoll[0].Returned;
end;

{ TRALChannel }

procedure TRALChannel.Close;
begin
  if FSocket <> nil then
    FSocket.Close;
end;

function TRALChannel.GetSocket: TRALSocket;
begin
  Result := FSocket;
end;

function TRALChannel.Flush: boolean;
begin
  Result := True;
end;

function TRALChannel.HasBuffered: boolean;
begin
  Result := False;
end;

function TRALChannel.ReadWait(var ABuffer; ACount, ATimeout: IntegerRAL): IntegerRAL;
var
  vDeadline, vNow: Int64RAL;
  vWait: IntegerRAL;
begin
  if ATimeout >= 0 then
    vDeadline := RALSocketNow + ATimeout
  else
    vDeadline := 0;
  repeat
    Result := Read(ABuffer, ACount);
    if Result <> RALSocketWouldBlock then
      Exit;
    if ATimeout >= 0 then
    begin
      vNow := RALSocketNow;
      if vNow >= vDeadline then
        Exit;
      vWait := vDeadline - vNow;
    end
    else
      vWait := -1;
    Socket.WaitFor(WantedEvents, vWait);
  until False;
end;

function TRALChannel.WantedEvents: Word;
begin
  Result := RALPollIn;
end;

procedure TRALChannel.WriteAll(const ABuffer; ACount, AStallTimeout: IntegerRAL);
var
  vData: PByte;
  vDone, vRes: IntegerRAL;
  vLastProgress: Int64RAL;
begin
  vData := @ABuffer;
  vDone := 0;
  vLastProgress := RALSocketNow;
  while (vDone < ACount) or not Flush do
  begin
    if vDone >= ACount then
    begin
      // all taken, part of it still queued in a layer
      if (AStallTimeout > 0) and (RALSocketNow - vLastProgress >= AStallTimeout) then
        raise ERALSocketError.CreateCode(0, Format(emSocketWriteStalled, [AStallTimeout]));
      Socket.WaitFor(RALPollOut, 100);
      Continue;
    end;
    vRes := Write(vData[vDone], ACount - vDone);
    if vRes > 0 then
    begin
      Inc(vDone, vRes);
      vLastProgress := RALSocketNow;
      Continue;
    end;
    // no progress: wait for room, but only as long as the link stays stuck
    if (AStallTimeout > 0) and (RALSocketNow - vLastProgress >= AStallTimeout) then
      raise ERALSocketError.CreateCode(0, Format(emSocketWriteStalled, [AStallTimeout]));
    if not Socket.Active then
      raise ERALSocketError.CreateCode(0, emSocketClosed);
    Socket.WaitFor(RALPollOut or RALPollIn, 100);
  end;
end;

{ TRALSocketChannel }

constructor TRALSocketChannel.Create(ASocket: TRALSocket; AOwnsSocket: boolean);
begin
  inherited Create;
  FSocket := ASocket;
  FOwnsSocket := AOwnsSocket;
end;

destructor TRALSocketChannel.Destroy;
begin
  if FOwnsSocket then
    FreeAndNil(FSocket);
  inherited Destroy;
end;

procedure TRALSocketChannel.Close;
begin
  FSocket.Close;
end;

function TRALSocketChannel.Read(var ABuffer; ACount: IntegerRAL): IntegerRAL;
begin
  Result := FSocket.Receive(ABuffer, ACount);
end;

function TRALSocketChannel.Write(const ABuffer; ACount: IntegerRAL): IntegerRAL;
begin
  Result := FSocket.Send(ABuffer, ACount);
end;

{ TRALLayerChannel }

constructor TRALLayerChannel.Create(AInner: TRALChannel; AOwnsInner: boolean);
begin
  inherited Create;
  FInner := AInner;
  FOwnsInner := AOwnsInner;
end;

destructor TRALLayerChannel.Destroy;
begin
  if FOwnsInner then
    FreeAndNil(FInner);
  inherited Destroy;
end;

procedure TRALLayerChannel.Close;
begin
  if FInner <> nil then
    FInner.Close;
end;

function TRALLayerChannel.GetSocket: TRALSocket;
begin
  Result := FInner.Socket;
end;

{$IFDEF RALWindows}
var
  vWSAData: array[0..1023] of Byte;

initialization
  WSAStartup($0202, @vWSAData);

finalization
  WSACleanup;
{$ENDIF}

end.
