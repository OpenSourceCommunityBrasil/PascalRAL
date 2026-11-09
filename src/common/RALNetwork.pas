{ Asks the operating system which IP addresses this machine has, with plain
  socket calls: no engine and no third-party library take part, so every
  server engine gets the same answer from TRALServer.GetServerAddress. }
unit RALNetwork;

{$I ..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes;

{ The address this machine uses on the network, in the given family: the
  source address the system picks for its default route, which is the one
  the other machines see - in IPv6 the stable one of that interface, not the
  temporary one the system prefers to send from. Without a default route
  (an isolated network, or no network at all) it is the first address of an
  interface that is up, link-local ones last, and with no such interface the
  loopback.
  '' only when the system refuses a socket of the family at all (no IPv6
  stack). Nothing is sent over the network to find it out. }
function RALGetLocalAddress(AMode: TRALIpMode = rimIPv4): StringRAL;
{ Every address of the family on an interface that is up, the loopback left
  out and link-local ones last. Clears AList first. Lists nothing on
  Android, whose older versions lack the call that walks the interfaces. }
procedure RALGetLocalAddresses(AMode: TRALIpMode; AList: TStrings);
{ True for the "every interface" address - '0.0.0.0', '::' or any other
  spelling of all zeros - and for an empty text, which the engines read the
  same way. }
function RALIsAnyAddress(const AAddress: StringRAL): boolean;
{ True when AValue is an address literal of the family: four dotted decimal
  numbers, or IPv6 hex groups (brackets and a %zone allowed). A host name,
  'localhost' included, is not. }
function RALIsIPAddress(const AValue: StringRAL; AMode: TRALIpMode): boolean;
/// The loopback of the family: '127.0.0.1' or '::1'
function RALLoopbackAddress(AMode: TRALIpMode): StringRAL;
{ A TCP port the system says is free now (a socket bound to port 0, read and
  closed): what a server with Port 0 listens on. Another program may take it
  before the server binds; 0 when the system gave none }
function RALFreeTCPPort(AMode: TRALIpMode = rimIPv4): IntegerRAL;

implementation

{$IFNDEF RALWindows}
uses
  {$IFDEF FPC}
  Sockets
  {$ELSE}
  Posix.Base, Posix.SysSocket, Posix.Unistd
  {$ENDIF};
{$ENDIF}

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ELSE}
  {$ALIGN 8}
{$ENDIF}

type
  { big enough for any sockaddr (sockaddr_storage is 128 bytes). The parts
    read here sit at the same offsets on every system - port at 2, an IPv4
    address at 4, an IPv6 one at 8 - only the family field moves }
  TRALSockAddrBuf = array[0..127] of Byte;

const
  { where the route probe "connects": the documentation ranges (RFC 5737 and
    RFC 3849), never assigned to anyone. Connecting a UDP socket only picks a
    route and a source address; no packet leaves the machine }
  cProbe4: array[0..3] of Byte = (192, 0, 2, 1);
  { An IPv6 address of the documentation prefix: a UDP connect to it finds the source
    address of the default route, sending nothing. }
  cProbe6: array[0..15] of Byte = ($20, $01, $0D, $B8, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 0, 0, 1);
  /// Port of the probe (discard).
  cProbePort = 9;
  /// Lowercase hex digits.
  cHexDigits: string = '0123456789abcdef';

{$IFDEF RALWindows}
const
  /// The Windows socket library.
  cWinsock = 'ws2_32.dll';
  /// AF_INET on Windows.
  cAF_INET = 2;
  /// AF_INET6 on Windows.
  cAF_INET6 = 23;
  /// SOCK_DGRAM.
  cSOCK_DGRAM = 2;

type
  /// A socket handle.
  TRALSock = NativeUInt;

  { the Winsock declarations are written here instead of taken from the RTL:
    Delphi and FPC ship different Winsock units, and older ones lack
    getaddrinfo }
  PRALAddrInfo = ^TRALAddrInfo;
  /// addrinfo (Windows).
  TRALAddrInfo = record
    /// Flags.
    ai_flags: Integer;
    /// Family.
    ai_family: Integer;
    /// Socket type.
    ai_socktype: Integer;
    /// Protocol.
    ai_protocol: Integer;
    /// Size of ai_addr.
    ai_addrlen: NativeUInt;
    /// Canonical name.
    ai_canonname: PAnsiChar;
    /// The address.
    ai_addr: Pointer;
    /// The next entry.
    ai_next: PRALAddrInfo;
  end;

/// Winsock WSAStartup.
function WSAStartup(AVersion: Word; AData: Pointer): Integer; stdcall;
  external cWinsock name 'WSAStartup';
/// Winsock WSACleanup.
function WSACleanup: Integer; stdcall;
  external cWinsock name 'WSACleanup';
/// Winsock socket.
function WinSocket(AFamily, AType, AProtocol: Integer): TRALSock; stdcall;
  external cWinsock name 'socket';
/// Winsock connect.
function WinConnect(ASocket: TRALSock; AAddr: Pointer; ALen: Integer): Integer; stdcall;
  external cWinsock name 'connect';
/// Winsock getsockname.
function WinGetSockName(ASocket: TRALSock; AAddr: Pointer; var ALen: Integer): Integer; stdcall;
  external cWinsock name 'getsockname';
/// Winsock closesocket.
function WinCloseSocket(ASocket: TRALSock): Integer; stdcall;
  external cWinsock name 'closesocket';
/// Winsock bind.
function WinBind(ASocket: TRALSock; AAddr: Pointer; ALen: Integer): Integer; stdcall;
  external cWinsock name 'bind';
/// Winsock gethostname.
function WinGetHostName(AName: PAnsiChar; ALen: Integer): Integer; stdcall;
  external cWinsock name 'gethostname';
/// Winsock getaddrinfo.
function WinGetAddrInfo(ANode, AService: PAnsiChar; AHints: PRALAddrInfo;
  var AResult: PRALAddrInfo): Integer; stdcall;
  external cWinsock name 'getaddrinfo';
/// Winsock freeaddrinfo.
procedure WinFreeAddrInfo(AInfo: PRALAddrInfo); stdcall;
  external cWinsock name 'freeaddrinfo';

/// The address family of AMode.
function FamilyOf(AMode: TRALIpMode): Integer;
begin
  if AMode = rimIPv6 then
    Result := cAF_INET6
  else
    Result := cAF_INET;
end;

/// Starts the socket library where one must be started; False when it refuses.
function NetStart: boolean;
var
  vData: array[0..1023] of Byte; // WSADATA, never read
begin
  Result := WSAStartup($0202, @vData) = 0;
end;

/// Ends what NetStart started.
procedure NetStop;
begin
  WSACleanup;
end;

/// Sets the family of a socket address.
procedure SetFamily(var ABuf: TRALSockAddrBuf; AFamily: Integer);
begin
  PWord(@ABuf)^ := Word(AFamily);
end;

/// The family of a socket address.
function GetFamily(const ABuf: TRALSockAddrBuf): Integer;
begin
  Result := PWord(@ABuf)^;
end;

/// Opens a UDP socket of AFamily.
function SysOpen(AFamily: Integer; out ASock: TRALSock): boolean;
begin
  ASock := WinSocket(AFamily, cSOCK_DGRAM, 0);
  Result := ASock <> not TRALSock(0); // INVALID_SOCKET
end;

/// Connects a socket to an address.
function SysConnect(ASock: TRALSock; const ABuf: TRALSockAddrBuf;
  ALen: Integer): boolean;
begin
  Result := WinConnect(ASock, @ABuf, ALen) = 0;
end;

/// The local address of a socket.
function SysGetSockName(ASock: TRALSock; var ABuf: TRALSockAddrBuf): boolean;
var
  vLen: Integer;
begin
  vLen := SizeOf(ABuf);
  Result := WinGetSockName(ASock, @ABuf, vLen) = 0;
end;

/// Closes a socket.
procedure SysClose(ASock: TRALSock);
begin
  WinCloseSocket(ASock);
end;

/// Opens a TCP socket of AFamily.
function SysOpenStream(AFamily: Integer; out ASock: TRALSock): boolean;
begin
  ASock := WinSocket(AFamily, 1 {SOCK_STREAM}, 0);
  Result := ASock <> not TRALSock(0);
end;

/// Binds a socket to an address.
function SysBind(ASock: TRALSock; const ABuf: TRALSockAddrBuf; ALen: Integer): boolean;
begin
  Result := WinBind(ASock, @ABuf, ALen) = 0;
end;

const
  /// The Windows IP helper library.
  cIphlpapi = 'iphlpapi.dll';
  /// Skip anycast, multicast, DNS servers and the friendly name.
  cGAAFlags = $2 or $4 or $8 or $20;
  /// ERROR_BUFFER_OVERFLOW.
  cERROR_BUFFER_OVERFLOW = 111;
  /// NL_SUFFIX_ORIGIN of a temporary address.
  cIpSuffixOriginRandom = 5;  // NL_SUFFIX_ORIGIN: a temporary address
  /// NL_DAD_STATE of a usable address.
  cIpDadStatePreferred = 4;

type
  /// Only the leading fields, laid out the same by every Windows since XP.
  TRALSocketAddress = record
    /// The address.
    lpSockaddr: Pointer;
    /// Its size.
    iSockaddrLength: Integer;
  end;

  /// Pointer to a TRALUnicastAddr.
  PRALUnicastAddr = ^TRALUnicastAddr;
  /// IP_ADAPTER_UNICAST_ADDRESS, the fields read.
  TRALUnicastAddr = record
    /// Length and Flags.
    Alignment: UInt64; // Length and Flags
    /// The next address.
    Next: PRALUnicastAddr;
    /// The address.
    Address: TRALSocketAddress;
    /// Where the prefix came from.
    PrefixOrigin: Integer;
    /// Where the suffix came from (random: temporary).
    SuffixOrigin: Integer;
    /// Duplicate address detection state.
    DadState: Integer;
  end;

  /// Pointer to a TRALAdapterAddr.
  PRALAdapterAddr = ^TRALAdapterAddr;
  /// IP_ADAPTER_ADDRESSES, the fields read.
  TRALAdapterAddr = record
    /// Length and IfIndex.
    Alignment: UInt64; // Length and IfIndex
    /// The next adapter.
    Next: PRALAdapterAddr;
    /// The adapter name.
    AdapterName: PAnsiChar;
    /// Its first address.
    FirstUnicastAddress: PRALUnicastAddr;
  end;

/// The Windows GetAdaptersAddresses.
function GetAdaptersAddresses(AFamily, AFlags: Cardinal; AReserved,
  AAddresses: Pointer; var ASize: Cardinal): Cardinal; stdcall;
  external cIphlpapi name 'GetAdaptersAddresses';
{$ELSE}
type
  /// A socket handle.
  TRALSock = Integer;

/// The address family of AMode.
function FamilyOf(AMode: TRALIpMode): Integer;
begin
  if AMode = rimIPv6 then
    Result := AF_INET6
  else
    Result := AF_INET;
end;

/// Starts the socket library where one must be started; False when it refuses.
function NetStart: boolean;
begin
  Result := True;
end;

/// Ends what NetStart started.
procedure NetStop;
begin
end;

{ sa_family is a word on Linux and Android, and a byte after sa_len on the
  BSDs and Apple; the RTL's own record knows which }
procedure SetFamily(var ABuf: TRALSockAddrBuf; AFamily: Integer);
begin
  psockaddr(@ABuf)^.sa_family := AFamily;
end;

/// The family of a socket address.
function GetFamily(const ABuf: TRALSockAddrBuf): Integer;
begin
  Result := psockaddr(@ABuf)^.sa_family;
end;

/// Opens a UDP socket of AFamily.
function SysOpen(AFamily: Integer; out ASock: TRALSock): boolean;
begin
  {$IFDEF FPC}
  ASock := fpSocket(AFamily, SOCK_DGRAM, 0);
  {$ELSE}
  ASock := socket(AFamily, SOCK_DGRAM, 0);
  {$ENDIF}
  Result := ASock >= 0;
end;

/// Connects a socket to an address.
function SysConnect(ASock: TRALSock; const ABuf: TRALSockAddrBuf;
  ALen: Integer): boolean;
begin
  {$IFDEF FPC}
  Result := fpConnect(ASock, psockaddr(@ABuf), ALen) = 0;
  {$ELSE}
  Result := connect(ASock, psockaddr(@ABuf)^, ALen) = 0;
  {$ENDIF}
end;

/// The local address of a socket.
function SysGetSockName(ASock: TRALSock; var ABuf: TRALSockAddrBuf): boolean;
var
  {$IFDEF FPC}
  vLen: TSockLen;
  {$ELSE}
  vLen: socklen_t;
  {$ENDIF}
begin
  vLen := SizeOf(ABuf);
  {$IFDEF FPC}
  Result := fpGetSockName(ASock, psockaddr(@ABuf), @vLen) = 0;
  {$ELSE}
  Result := getsockname(ASock, psockaddr(@ABuf)^, vLen) = 0;
  {$ENDIF}
end;

/// Closes a socket.
procedure SysClose(ASock: TRALSock);
begin
  {$IFDEF FPC}
  CloseSocket(ASock);
  {$ELSE}
  __close(ASock);
  {$ENDIF}
end;

/// Opens a TCP socket of AFamily.
function SysOpenStream(AFamily: Integer; out ASock: TRALSock): boolean;
begin
  {$IFDEF FPC}
  ASock := fpSocket(AFamily, SOCK_STREAM, 0);
  {$ELSE}
  ASock := socket(AFamily, SOCK_STREAM, 0);
  {$ENDIF}
  Result := ASock >= 0;
end;

/// Binds a socket to an address.
function SysBind(ASock: TRALSock; const ABuf: TRALSockAddrBuf; ALen: Integer): boolean;
begin
  {$IFDEF FPC}
  Result := fpBind(ASock, psockaddr(@ABuf), ALen) = 0;
  {$ELSE}
  Result := bind(ASock, psockaddr(@ABuf)^, ALen) = 0;
  {$ENDIF}
end;

{$IFNDEF ANDROID}
const
  /// IFF_UP: the interface is up.
  cIFF_UP = $1;

type
  /// Struct ifaddrs, the same on Linux and on Apple.
  PRALIfAddrs = ^TRALIfAddrs;
  /// struct ifaddrs.
  TRALIfAddrs = record
    /// The next entry.
    ifa_next: PRALIfAddrs;
    /// The interface name.
    ifa_name: PAnsiChar;
    /// The IFF_* flags.
    ifa_flags: Cardinal;
    /// The address.
    ifa_addr: Pointer;
    /// The netmask.
    ifa_netmask: Pointer;
    /// Broadcast or destination address.
    ifa_ifu: Pointer;
    /// Address family data.
    ifa_data: Pointer;
  end;

{$IFDEF FPC}
/// libc getifaddrs.
function getifaddrs(var AList: PRALIfAddrs): Integer; cdecl;
  external 'c' name 'getifaddrs';
/// libc freeifaddrs.
procedure freeifaddrs(AList: PRALIfAddrs); cdecl;
  external 'c' name 'freeifaddrs';
{$ELSE}
/// libc getifaddrs.
function getifaddrs(var AList: PRALIfAddrs): Integer; cdecl;
  external libc name _PU + 'getifaddrs';
/// libc freeifaddrs.
procedure freeifaddrs(AList: PRALIfAddrs); cdecl;
  external libc name _PU + 'freeifaddrs';
{$ENDIF}
{$ENDIF}
{$ENDIF}

/// Size of a socket address of AMode.
function AddrLen(AMode: TRALIpMode): Integer;
begin
  if AMode = rimIPv6 then
    Result := 28 // sockaddr_in6
  else
    Result := 16; // sockaddr_in
end;

/// The text of the address in ABuf.
function FormatAddress(const ABuf: TRALSockAddrBuf; AMode: TRALIpMode): string;
var
  vGroups: array[0..7] of Word;
  vInt, vRunStart, vRunLen, vBestStart, vBestLen: Integer;
  vWord: Word;
  vHex: string;
begin
  if AMode = rimIPv4 then
  begin
    Result := IntToStr(ABuf[4]) + '.' + IntToStr(ABuf[5]) + '.' +
              IntToStr(ABuf[6]) + '.' + IntToStr(ABuf[7]);
    Exit;
  end;

  for vInt := 0 to 7 do
    vGroups[vInt] := (ABuf[8 + vInt * 2] shl 8) or ABuf[9 + vInt * 2];

  { RFC 5952: the longest run of two or more zero groups becomes '::', the
    first one on a tie; hex in lower case without leading zeros }
  vBestStart := -1;
  vBestLen := 1;
  vRunStart := -1;
  vRunLen := 0;
  for vInt := 0 to 7 do
  begin
    if vGroups[vInt] = 0 then
    begin
      if vRunStart < 0 then
      begin
        vRunStart := vInt;
        vRunLen := 0;
      end;
      Inc(vRunLen);
      if vRunLen > vBestLen then
      begin
        vBestStart := vRunStart;
        vBestLen := vRunLen;
      end;
    end
    else
      vRunStart := -1;
  end;

  Result := '';
  vInt := 0;
  while vInt <= 7 do
  begin
    if vInt = vBestStart then
    begin
      Result := Result + '::';
      Inc(vInt, vBestLen);
      Continue;
    end;
    if (vInt > 0) and (vInt <> vBestStart + vBestLen) then
      Result := Result + ':';
    vWord := vGroups[vInt];
    vHex := '';
    repeat
      vHex := cHexDigits[(vWord and $F) + 1] + vHex;
      vWord := vWord shr 4;
    until vWord = 0;
    Result := Result + vHex;
    Inc(vInt);
  end;
end;

/// Whether ABuf is the any address or a loopback one.
function IsAnyOrLoopback(const ABuf: TRALSockAddrBuf; AMode: TRALIpMode): boolean;
var
  vInt: Integer;
begin
  if AMode = rimIPv4 then
  begin
    Result := (ABuf[4] = 127) or
              ((ABuf[4] = 0) and (ABuf[5] = 0) and (ABuf[6] = 0) and (ABuf[7] = 0));
    Exit;
  end;
  // :: and ::1 - fifteen zero bytes, then 0 or 1
  Result := ABuf[23] <= 1;
  for vInt := 8 to 22 do
    if ABuf[vInt] <> 0 then
    begin
      Result := False;
      Break;
    end;
end;

/// Whether ABuf is a link-local address.
function IsLinkLocal(const ABuf: TRALSockAddrBuf; AMode: TRALIpMode): boolean;
begin
  if AMode = rimIPv4 then
    Result := (ABuf[4] = 169) and (ABuf[5] = 254) // 169.254/16, no DHCP answer
  else
    Result := (ABuf[8] = $FE) and ((ABuf[9] and $C0) = $80); // fe80::/10
end;

/// Adds the address in ABuf to APreferred or ALinkLocal, unless it is any or loopback.
procedure AddCandidate(const ABuf: TRALSockAddrBuf; AMode: TRALIpMode;
  APreferred, ALinkLocal: TStrings);
var
  vText: string;
begin
  if IsAnyOrLoopback(ABuf, AMode) then
    Exit;
  vText := FormatAddress(ABuf, AMode);
  if IsLinkLocal(ABuf, AMode) then
  begin
    if ALinkLocal.IndexOf(vText) < 0 then
      ALinkLocal.Add(vText);
  end
  else if APreferred.IndexOf(vText) < 0 then
    APreferred.Add(vText);
end;

{$IFDEF RALWindows}
{ the addresses the system answers for its own host name: one per adapter
  that is up }
procedure SysListAddresses(AMode: TRALIpMode; APreferred, ALinkLocal: TStrings);
var
  vName: array[0..255] of AnsiChar;
  vHints: TRALAddrInfo;
  vList, vItem: PRALAddrInfo;
  vBuf: TRALSockAddrBuf;
  vLen: NativeUInt;
begin
  FillChar(vName, SizeOf(vName), 0);
  if WinGetHostName(@vName[0], SizeOf(vName) - 1) <> 0 then
    Exit;

  FillChar(vHints, SizeOf(vHints), 0);
  vHints.ai_family := FamilyOf(AMode);
  vHints.ai_socktype := cSOCK_DGRAM; // one entry per address, not per protocol
  vList := nil;
  if WinGetAddrInfo(@vName[0], nil, @vHints, vList) <> 0 then
    Exit;
  try
    vItem := vList;
    while vItem <> nil do
    begin
      if (vItem^.ai_family = vHints.ai_family) and (vItem^.ai_addr <> nil) then
      begin
        FillChar(vBuf, SizeOf(vBuf), 0);
        vLen := vItem^.ai_addrlen;
        if vLen > SizeOf(vBuf) then
          vLen := SizeOf(vBuf);
        Move(vItem^.ai_addr^, vBuf, vLen);
        AddCandidate(vBuf, AMode, APreferred, ALinkLocal);
      end;
      vItem := vItem^.ai_next;
    end;
  finally
    WinFreeAddrInfo(vList);
  end;
end;
{$ELSE}
{$IFDEF ANDROID}
/// Lists the addresses of the interfaces that are up.
procedure SysListAddresses(AMode: TRALIpMode; APreferred, ALinkLocal: TStrings);
begin
  // getifaddrs only exists from Android 7 on, and linking it would stop the
  // whole application from loading on anything older
end;
{$ELSE}
/// Lists the addresses of the interfaces that are up.
procedure SysListAddresses(AMode: TRALIpMode; APreferred, ALinkLocal: TStrings);
var
  vList, vItem: PRALIfAddrs;
  vBuf: TRALSockAddrBuf;
begin
  vList := nil;
  if getifaddrs(vList) <> 0 then
    Exit;
  try
    vItem := vList;
    while vItem <> nil do
    begin
      if (vItem^.ifa_addr <> nil) and ((vItem^.ifa_flags and cIFF_UP) <> 0) then
      begin
        FillChar(vBuf, SizeOf(vBuf), 0);
        // the family first: an AF_PACKET or AF_LINK entry is shorter than 28
        Move(vItem^.ifa_addr^, vBuf, 2);
        if GetFamily(vBuf) = FamilyOf(AMode) then
        begin
          Move(vItem^.ifa_addr^, vBuf, AddrLen(AMode));
          AddCandidate(vBuf, AMode, APreferred, ALinkLocal);
        end;
      end;
      vItem := vItem^.ifa_next;
    end;
  finally
    freeifaddrs(vList);
  end;
end;
{$ENDIF}
{$ENDIF}

{ RFC 6724 rule 7 makes the source address of the IPv6 probe the TEMPORARY one
  wherever privacy extensions are on - Windows and macOS by default, most Linux
  desktops, Android - and a temporary address rotates within a day and stops
  being accepted soon after: a server publishing it is unreachable at it a day
  later. StableIPv6 answers the stable address of the same interface and /64
  (SLAAC, which is where temporary addresses come from, always uses that
  prefix length), or '' to keep what the probe found: the probe's address is
  not temporary, or nothing better is there, or the system will not say. }
{$IFDEF RALWindows}
/// The address of AUni as a sockaddr in ABuf; False for any other family.
function UnicastAddress(AUni: PRALUnicastAddr; var ABuf: TRALSockAddrBuf): boolean;
begin
  Result := (AUni^.Address.lpSockaddr <> nil) and
            (AUni^.Address.iSockaddrLength >= 24) and
            (AUni^.Address.iSockaddrLength <= SizeOf(ABuf));
  if not Result then
    Exit;
  FillChar(ABuf, SizeOf(ABuf), 0);
  Move(AUni^.Address.lpSockaddr^, ABuf, AUni^.Address.iSockaddrLength);
  Result := GetFamily(ABuf) = cAF_INET6;
end;

/// The stable (not temporary) IPv6 address of the interface of AAddr, or empty.
function StableIPv6(const AAddr: TRALSockAddrBuf): string;
var
  vData: array of Byte;
  vSize, vError: Cardinal;
  vTry: Integer;
  vAdapter: PRALAdapterAddr;
  vUni: PRALUnicastAddr;
  vBuf: TRALSockAddrBuf;
begin
  Result := '';
  vSize := 16384;
  vTry := 0;
  repeat
    SetLength(vData, vSize);
    vError := GetAdaptersAddresses(cAF_INET6, cGAAFlags, nil, @vData[0], vSize);
    Inc(vTry);
  until (vError <> cERROR_BUFFER_OVERFLOW) or (vTry = 3);
  if vError <> 0 then
    Exit;

  vAdapter := @vData[0];
  while vAdapter <> nil do
  begin
    vUni := vAdapter^.FirstUnicastAddress;
    while (vUni <> nil) and
          not (UnicastAddress(vUni, vBuf) and CompareMem(@vBuf[8], @AAddr[8], 16)) do
      vUni := vUni^.Next;

    if vUni <> nil then
    begin
      if vUni^.SuffixOrigin <> cIpSuffixOriginRandom then
        Exit;
      vUni := vAdapter^.FirstUnicastAddress;
      while vUni <> nil do
      begin
        if (vUni^.SuffixOrigin <> cIpSuffixOriginRandom) and
           (vUni^.DadState = cIpDadStatePreferred) and UnicastAddress(vUni, vBuf) and
           CompareMem(@vBuf[8], @AAddr[8], 8) then
        begin
          Result := FormatAddress(vBuf, rimIPv6);
          Exit;
        end;
        vUni := vUni^.Next;
      end;
      Exit;
    end;
    vAdapter := vAdapter^.Next;
  end;
end;
{$ELSE}
{ /proc/net/if_inet6, one line per address: 32 hex digits, then interface
  index, prefix length, scope and flags in hex, then the interface name.
  There on Linux; on Android only up to 9, later ones refuse it to apps; not
  on Apple. Where it cannot be read the probe's answer stands }
function StableIPv6(const AAddr: TRALSockAddrBuf): string;
const
  cTemporary = $01;
  cUnusable = cTemporary or $08 or $20 or $40; // dadfailed, deprecated, tentative
type
  TRALIfInet6 = record
    /// The address, from /proc/net/if_inet6.
    Addr: array[0..15] of Byte;
    /// Interface index and IFA_F_* flags.
    IfIndex, Flags: Integer;
  end;
var
  vFile: TextFile;
  vLine: string;
  vItems: array of TRALIfInet6;
  vItem: TRALIfInet6;
  vInt, vMine: Integer;
  vParts: TStringList;
  vBuf: TRALSockAddrBuf;
begin
  Result := '';
  SetLength(vItems, 0);
  AssignFile(vFile, '/proc/net/if_inet6');
  {$I-}
  Reset(vFile);
  {$I+}
  if IOResult <> 0 then
    Exit;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ' ';
    vParts.StrictDelimiter := False;
    while not Eof(vFile) do
    begin
      Readln(vFile, vLine);
      vParts.DelimitedText := vLine;
      if (vParts.Count < 5) or (Length(vParts[0]) <> 32) then
        Continue;
      for vInt := 0 to 15 do
        vItem.Addr[vInt] := StrToIntDef('$' + Copy(vParts[0], vInt * 2 + 1, 2), 0);
      vItem.IfIndex := StrToIntDef('$' + vParts[1], -1);
      vItem.Flags := StrToIntDef('$' + vParts[4], cUnusable);
      SetLength(vItems, Length(vItems) + 1);
      vItems[High(vItems)] := vItem;
    end;
  finally
    vParts.Free;
    CloseFile(vFile);
  end;

  vMine := -1;
  for vInt := 0 to High(vItems) do
    if CompareMem(@vItems[vInt].Addr[0], @AAddr[8], 16) then
    begin
      vMine := vInt;
      Break;
    end;
  if (vMine < 0) or ((vItems[vMine].Flags and cTemporary) = 0) then
    Exit;

  for vInt := 0 to High(vItems) do
    if (vItems[vInt].IfIndex = vItems[vMine].IfIndex) and
       ((vItems[vInt].Flags and cUnusable) = 0) and
       CompareMem(@vItems[vInt].Addr[0], @AAddr[8], 8) then
    begin
      vBuf := AAddr;
      Move(vItems[vInt].Addr[0], vBuf[8], 16);
      Result := FormatAddress(vBuf, rimIPv6);
      Exit;
    end;
end;
{$ENDIF}

/// Fills AList with the addresses of AMode, link-local ones last.
procedure ListAddresses(AMode: TRALIpMode; AList: TStrings);
var
  vLinkLocal: TStringList;
begin
  AList.Clear;
  vLinkLocal := TStringList.Create;
  try
    SysListAddresses(AMode, AList, vLinkLocal);
    AList.AddStrings(vLinkLocal);
  finally
    vLinkLocal.Free;
  end;
end;

{ the source address the system picks to reach an address outside the
  machine, that is, the one of the default route. AAvailable says whether
  the family exists here at all }
function ProbeRoute(AMode: TRALIpMode; out AAvailable: boolean): string;
var
  vSock: TRALSock;
  vBuf: TRALSockAddrBuf;
begin
  Result := '';
  AAvailable := SysOpen(FamilyOf(AMode), vSock);
  if not AAvailable then
    Exit;
  try
    FillChar(vBuf, SizeOf(vBuf), 0);
    SetFamily(vBuf, FamilyOf(AMode));
    vBuf[2] := (cProbePort shr 8) and $FF; // network order
    vBuf[3] := cProbePort and $FF;
    if AMode = rimIPv6 then
      Move(cProbe6, vBuf[8], SizeOf(cProbe6))
    else
      Move(cProbe4, vBuf[4], SizeOf(cProbe4));
    if not SysConnect(vSock, vBuf, AddrLen(AMode)) then
      Exit; // no route: an isolated network, or none at all

    FillChar(vBuf, SizeOf(vBuf), 0);
    if SysGetSockName(vSock, vBuf) and (GetFamily(vBuf) = FamilyOf(AMode)) and
       (not IsAnyOrLoopback(vBuf, AMode)) then
    begin
      if AMode = rimIPv6 then
        Result := StableIPv6(vBuf);
      if Result = '' then
        Result := FormatAddress(vBuf, AMode);
    end;
  finally
    SysClose(vSock);
  end;
end;

/// AValue without blanks around it and without the brackets of a URL.
function CleanAddress(const AValue: StringRAL): string;
var
  vFirst, vLast: Integer;
begin
  Result := string(AValue);
  vFirst := 1;
  vLast := Length(Result);
  while (vFirst <= vLast) and (Result[vFirst] <= ' ') do
    Inc(vFirst);
  while (vLast >= vFirst) and (Result[vLast] <= ' ') do
    Dec(vLast);
  if (vLast > vFirst) and (Result[vFirst] = '[') and (Result[vLast] = ']') then
  begin
    Inc(vFirst);
    Dec(vLast);
  end;
  Result := Copy(Result, vFirst, vLast - vFirst + 1);
end;

function RALGetLocalAddress(AMode: TRALIpMode): StringRAL;
var
  vAvailable: boolean;
  vList: TStringList;
begin
  Result := '';
  if not NetStart then
    Exit;
  try
    Result := StringRAL(ProbeRoute(AMode, vAvailable));
    if (Result <> '') or (not vAvailable) then
      Exit;

    vList := TStringList.Create;
    try
      ListAddresses(AMode, vList);
      if vList.Count > 0 then
        Result := StringRAL(vList[0])
      else
        Result := RALLoopbackAddress(AMode);
    finally
      vList.Free;
    end;
  finally
    NetStop;
  end;
end;

procedure RALGetLocalAddresses(AMode: TRALIpMode; AList: TStrings);
begin
  AList.Clear;
  if not NetStart then
    Exit;
  try
    ListAddresses(AMode, AList);
  finally
    NetStop;
  end;
end;

function RALIsAnyAddress(const AAddress: StringRAL): boolean;
var
  vText: string;
  vInt: Integer;
begin
  vText := CleanAddress(AAddress);
  Result := True;
  for vInt := 1 to Length(vText) do
    if (vText[vInt] <> '0') and (vText[vInt] <> '.') and (vText[vInt] <> ':') then
    begin
      Result := False;
      Break;
    end;
end;

function RALIsIPAddress(const AValue: StringRAL; AMode: TRALIpMode): boolean;
var
  vText: string;
  vInt, vParts, vDigits, vNumber, vColons: Integer;
  vChar: Char;
begin
  vText := CleanAddress(AValue);
  Result := False;
  if vText = '' then
    Exit;

  if AMode = rimIPv4 then
  begin
    vParts := 1;
    vDigits := 0;
    vNumber := 0;
    for vInt := 1 to Length(vText) do
    begin
      vChar := vText[vInt];
      if vChar = '.' then
      begin
        if vDigits = 0 then
          Exit;
        Inc(vParts);
        vDigits := 0;
        vNumber := 0;
      end
      else if (vChar >= '0') and (vChar <= '9') then
      begin
        Inc(vDigits);
        vNumber := vNumber * 10 + Ord(vChar) - Ord('0');
        if (vDigits > 3) or (vNumber > 255) then
          Exit;
      end
      else
        Exit;
    end;
    Result := (vParts = 4) and (vDigits > 0);
    Exit;
  end;

  // a zone ("fe80::1%eth0") names the local interface, not the address
  vInt := Pos('%', vText);
  if vInt > 0 then
    vText := Copy(vText, 1, vInt - 1);
  vColons := 0;
  for vInt := 1 to Length(vText) do
  begin
    vChar := vText[vInt];
    if vChar = ':' then
      Inc(vColons)
    else if not (((vChar >= '0') and (vChar <= '9')) or
                 ((vChar >= 'a') and (vChar <= 'f')) or
                 ((vChar >= 'A') and (vChar <= 'F')) or (vChar = '.')) then
      Exit;
  end;
  // "::" at most once, and a colon is what no host name can hold
  Result := (vColons >= 2) and (vColons <= 7) and
            (Pos('::', Copy(vText, Pos('::', vText) + 1, Length(vText))) = 0);
end;

function RALFreeTCPPort(AMode: TRALIpMode): IntegerRAL;
var
  vSock: TRALSock;
  vBuf: TRALSockAddrBuf;
  vLen: Integer;
begin
  Result := 0;
  if not NetStart then
    Exit;
  try
    if not SysOpenStream(FamilyOf(AMode), vSock) then
      Exit;
    try
      { every address, port 0: the system picks the port }
      FillChar(vBuf, SizeOf(vBuf), 0);
      SetFamily(vBuf, FamilyOf(AMode));
      if AMode = rimIPv6 then
        vLen := 28
      else
        vLen := 16;
      if not SysBind(vSock, vBuf, vLen) then
        Exit;
      FillChar(vBuf, SizeOf(vBuf), 0);
      if SysGetSockName(vSock, vBuf) then
        Result := (Integer(vBuf[2]) shl 8) or vBuf[3]; // network byte order
    finally
      SysClose(vSock);
    end;
  finally
    NetStop;
  end;
end;

function RALLoopbackAddress(AMode: TRALIpMode): StringRAL;
begin
  if AMode = rimIPv6 then
    Result := '::1'
  else
    Result := '127.0.0.1';
end;

end.
