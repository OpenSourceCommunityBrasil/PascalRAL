# HTTP/2 in PascalRAL

Everything HTTP/2 brought into RAL, and everything that came in beside it: what each
new property does, which engine honours which, what is only ever *observed*, what
raises and what is silently ignored.

Written against the seven commits from `6ac4eb7` to `2739f73` (2026-09-15). The
OkHttp engine has its own page for wiring the jars into a project -
[`okhttp/README.md`](okhttp/README.md); this one is about the protocol and the knobs.

---

## Contents

- [The short version](#the-short-version)
- [How HTTP/2 happens here](#how-http2-happens-here)
- [TRALHTTPVersion: one type, two directions](#tralhttpversion-one-type-two-directions)
- [Reading back what was negotiated](#reading-back-what-was-negotiated)
- [What each engine can do](#what-each-engine-can-do)
- [The client: HTTPVersion](#the-client-httpversion)
- [The client: ShareConnection](#the-client-shareconnection)
- [ShareConnection x HTTPVersion, measured](#shareconnection-x-httpversion-measured)
- [The client: KeepAliveInterval](#the-client-keepaliveinterval)
- [The server: TRALSynopseServer.Mode](#the-server-tralsynopseservermode)
- [Serving HTTP/2: the http.sys checklist](#serving-http2-the-httpsys-checklist)
- [MaxConnections, now the same name on four servers](#maxconnections-now-the-same-name-on-four-servers)
- [What raises, and what is quietly ignored](#what-raises-and-what-is-quietly-ignored)
- [The Object Inspector hides what does not apply](#the-object-inspector-hides-what-does-not-apply)
- [Troubleshooting](#troubleshooting)
- [Upgrading from an older RAL](#upgrading-from-an-older-ral)
- [The commits](#the-commits)

---

## The short version

**Client** (Windows, netHTTP):

```pascal
RALClient.EngineType      := 'netHTTP';
RALClient.BaseURL.Text    := 'https://server:9988';
RALClient.HTTPVersion     := rhv2;    // ask for h2
RALClient.ShareConnection := True;    // and let the clients share one connection
```

**Client** (Android, OkHttp):

```pascal
RALClient.EngineType      := 'OkHttp';
RALClient.BaseURL.Text    := 'https://server:9988';
RALClient.HTTPVersion     := rhv2;
RALClient.ShareConnection := True;
RALClient.KeepAliveInterval := RALClient.ConnectTimeout div 2;  // notice a dead peer
```

**Server** (Windows, mORMot2):

```pascal
RALServer.Mode          := smHttpSys;   // the only mode that serves HTTP/2
RALServer.HttpSysDomain := 'localhost'; // or '+' to accept from anywhere
RALServer.SSL.Enabled   := True;        // the certificate itself comes from netsh
```

**Check that it worked** - on the server, which is the only side that always knows:

```pascal
if Request.ProtocolVersion = rhv2 then ...   // or: Request.Protocol = '2.0'
```

---

## How HTTP/2 happens here

**RAL frames no HTTP/2 itself.** What frames it is the library under an engine:
WinHTTP on Windows, OkHttp on Android, http.sys in the Windows kernel on the server
side. RAL's whole job is to *ask* for it, to *report* what was settled, and to refuse
clearly when an engine cannot.

It is settled by **ALPN, inside the TLS handshake**, before any request exists. Three
consequences follow, and all three surprise someone eventually:

1. **It is TLS-only.** Neither side offers the cleartext upgrade (`h2c`). `http://` is
   HTTP/1.1 whatever `HTTPVersion` says.
2. **Asking is not getting.** A server without h2 answers 1.1 to a client that asked
   for 2, and that is a normal, successful request - not an error. Which is why
   `HTTPVersion` (what you asked) and `ProtocolVersion` (what happened) are two
   different properties on two different objects.
3. **The negotiation is per connection, not per request.** Two `BaseURL`s may well
   settle differently, so the answer is reported on each response and never cached on
   the client.

What HTTP/2 buys, in one line: **one connection carries many requests at the same
time** (multiplexing), instead of one request at a time per connection with everything
else queued behind it.

---

## TRALHTTPVersion: one type, two directions

Declared in [`../base/RALTypes.pas`](../base/RALTypes.pas):

```pascal
TRALHTTPVersion = (rhvDefault, rhv10, rhv11, rhv2);
```

| Value | As a request (`TRALClient.HTTPVersion`) | As an observation (`ProtocolVersion`) |
|---|---|---|
| `rhvDefault` | leave the engine exactly as it was - the default, so nothing changes for code that does not ask | the transport could not tell |
| `rhv10` | **refused** - raises `emHTTP10NotRequestable` | HTTP/1.0 really arrived |
| `rhv11` | force 1.1, declining an h2 the platform might have taken | the message travelled on 1.1 |
| `rhv2` | ask for h2, falling back to 1.1 if the server has none | the message travelled on h2 |

`rhv10` exists **only** because a server receives it: no engine exposes a switch to
send HTTP/1.0, so accepting it as a request would mean sending 1.1 and reporting 1.0 -
precisely the disagreement this type exists to rule out. The fpHTTP server, for one,
decides whether to close the connection by it.

Two helpers convert, in the same unit:

```pascal
function RALHTTPVersionToStr(AVersion: TRALHTTPVersion): StringRAL;   // '' '1.0' '1.1' '2.0'
function StrToRALHTTPVersion(const AValue: StringRAL): TRALHTTPVersion;
```

`StrToRALHTTPVersion` is deliberately forgiving about its input, because it reads
whatever an engine happens to have: a whole status line (`HTTP/1.1 200 OK`), a bare
version (`1.1`, `2`), or an ALPN token (`h2`, `h2c`). Anything it cannot parse comes
back as `rhvDefault` - it never guesses.

---

## Reading back what was negotiated

On `TRALHTTPHeaderInfo` - so on `TRALRequest` at the server, on `TRALResponse` at the
client - in [`../utils/RALCustomObjects.pas`](../utils/RALCustomObjects.pas):

```pascal
property ProtocolVersion: TRALHTTPVersion read FProtocolVersion write FProtocolVersion;
property Protocol: StringRAL read GetProtocol write SetProtocol stored False;
```

**They are two faces of one field.** One `FProtocolVersion` behind both: an engine
that fills either one has filled both, and they can never disagree. Write text that no
version parses out of and you get `rhvDefault` back, so `Protocol` reads as `''` - it
is not a free-text field. `Protocol` is `stored False` because the typed one is what
gets streamed.

Use whichever fits the code you are writing: `Protocol` for a log line or a header,
`ProtocolVersion` when you are branching.

**Where the answer is reliable:**

| Side | Reliable? | Why |
|---|---|---|
| **Server**, `smHttpSys` | yes | read from the driver that negotiated it - and from the flag `HTTP_REQUEST_FLAG_HTTP2`, **not** from `HTTP_REQUEST.Version`, which reports 1.1 even for an h2 request. That trap cost one wrong measurement before it was found |
| **Server**, other engines | yes | parsed from the request line, which HTTP/1.x always has |
| **Client**, netHTTP on Windows | yes | asked of WinHTTP directly, under `WINHTTP_OPTION_HTTP_PROTOCOL_USED` |
| **Client**, netHTTP elsewhere | partly | falls back to the RTL's `IHTTPResponse.Version`, which reads the **status line** - and an HTTP/2 response does not have one, so it under-reports h2 as 1.1 |
| **Client**, OkHttp | yes | OkHttp reports the protocol of the call itself |

If you only instrument one side, **instrument the server**.

---

## What each engine can do

Four `class function`s on `TRALClientHTTP` answer, so a caller can ask *before*
sending ([`../base/RALClient.pas`](../base/RALClient.pas)):

```pascal
class function SupportsCertPin: boolean; virtual;
class function SupportsHTTP2: boolean; virtual;
class function SupportsSharedConnection: boolean; virtual;
class function SupportsKeepAliveInterval: boolean; virtual;
class function MinKeepAliveInterval: IntegerRAL; virtual;
```

They are class functions, not properties, because `TRALClient` is **one** component
with a pluggable `EngineType` - the question has to be answerable at design time, when
there is no engine instance at all.

### Clients

| `EngineType` | HTTP/2 | ShareConnection | KeepAliveInterval | CertPin (`SSL.Pins`) |
|---|---|---|---|---|
| `'netHTTP'` | **yes** - see the note below; not Android | **yes** | **yes** - Windows 11 and newer | Windows only |
| `'OkHttp'` | **yes** - Android only | **yes** - Android only | **yes** - Android only | Android only |
| `'Indy'` | no | no | no | yes, everywhere |
| `'mORMot2'` | no | no | no | yes (with OpenSSL; SChannel is caught right after the handshake) |
| `'fpHTTP'` | no | no | no | yes |

Three notes on the asymmetries:

- **netHTTP asks the RTL, not a version table.** Its `SupportsHTTP2` is compiled in
  when `THTTPProtocolVersion` is declared in the RTL being built against
  (`{$IF Declared(THTTPProtocolVersion)}`), which answers the only question that
  matters and stays right as releases come and go. On an RTL without it the engine
  behaves exactly as it always did, and `rhv2` is refused rather than ignored.
- **netHTTP has no h2 on Android**, though it is the same RAL unit: `TNetHTTPClient`
  there lands on `HttpURLConnection`, whose bundled copy of OkHttp is handed a
  protocol list without h2. That is the entire reason the OkHttp engine exists.
- **OkHttp is registered on every platform and refuses to send on all but Android.**
  It has to be: `EngineType`'s property editor lists whatever `RegisterEngine` filled
  in, and an engine wrapped in `{$IFDEF ANDROID}` compiles to nothing on the IDE's own
  platform, so its name could never be picked. Elsewhere `SendUrl` raises
  `emOkHttpAndroidOnly`.

### Servers

| Server | HTTP/2 |
|---|---|
| `TRALSynopseServer`, `Mode = smHttpSys` | **yes** - the Windows kernel serves it |
| `TRALSynopseServer`, `smThreads` / `smAsync` | no - mORMot2 has no HTTP/2 of its own |
| Indy, fpHTTP, Sagui, UniGUI, CGI | no |

---

## The client: HTTPVersion

```pascal
property HTTPVersion: TRALHTTPVersion read FHTTPVersion write FHTTPVersion
  default rhvDefault;
```

What the client **asks** the transport for. It is a request, not a guarantee - read
`Response.ProtocolVersion` to learn what happened.

`rhv2` on an engine whose `SupportsHTTP2` is False **raises on the first request**
(`emHTTP2Unsupported`), before a socket is opened, instead of falling back in silence.
A transport that quietly is not what was asked for is how one spends an afternoon
wondering why nothing got faster.

One behaviour change worth knowing: **under `rhv2` RAL stops sending the `Connection`
header.** It is one of the connection-specific headers HTTP/2 forbids (RFC 7540,
8.1.2.2) - a request carrying it is malformed, and a strict peer answers
`PROTOCOL_ERROR` instead of the resource. It is also pointless there, an h2 connection
being persistent by definition. Under `rhvDefault` and `rhv11` the header stays
exactly as it was.

---

## The client: ShareConnection

```pascal
property ShareConnection: boolean read FShareConnection write FShareConnection
  default False;
```

Lets this client share its underlying transport - and therefore its TCP connection -
with every other client aimed at the same host with the same settings.

**What it is for.** RAL asks for one `TRALClient` per dataset, because `Request` is one
object per client. Without sharing, an application with 40 datasets opens 40
connections and pays 40 cold TCP + TLS handshakes.

**It is a hint, not a contract.** Engines that cannot share ignore it silently instead
of raising, because the same client is often configured once and run over a different
engine per platform; refusing there would turn an optimisation into a portability
problem.

**What it changes, and why it is off by default:**

- the engine's cookie jar becomes common to the sharers;
- their requests queue on one connection **unless the transport can multiplex** -
  which is exactly what `HTTPVersion = rhv2` buys.

**Only clients that judge certificates alike ever share.** The certificate policy -
`SSL.Verify`, `SSL.Pins`, and the code *and instance* of `OnValidateServerCert` - is
part of the pool key on both engines that implement sharing. Without that, a client
with a pin could inherit a TLS connection someone else opened under a looser rule, and
its pin would never run.

How each engine does it:

- **netHTTP** keeps a transport pool of its own, keyed by
  `authority | user-agent | connect timeout | request timeout | max redirects | version | cert policy`.
- **OkHttp** hands the question to OkHttp's own client cache, keyed by
  `cert policy | connect | read | ping | h2 | redirects`.

---

## ShareConnection x HTTPVersion, measured

The two are **not** alternatives, and neither does the other's job. Measured on
Windows with a purpose-built harness: 40 clients, 1600 requests, 2 in flight at a
time, netHTTP against a `smHttpSys` server over TLS, zero errors.

| `ShareConnection` | `HTTPVersion` | connections opened | peak simultaneous | total ms |
|---|---|---|---|---|
| **True** | **`rhv2`** | **1** | **1** | **3545** |
| True | `rhv11` | 2 | 2 | 4680 |
| False | `rhv2` | 59 | 42 | 11309 |
| False | `rhv11` | 53 | 2 | 7925 |

Reading it:

- **Sharing saves handshakes.** Under 1.1 it took 53 connections down to 2 - but the
  peak stayed at 2, because 1.1 still needs one connection per request in flight.
- **h2 saves simultaneous connections.** With sharing on, 2 became 1: both requests
  ride the same connection as separate streams.
- **`False` + `rhv2` is the worst cell of the four** - slower even than 1.1 without
  sharing. Each unshared client gets its own transport, none of them capped, and h2
  connections stay open: the cost of h2 with none of its point. **If you turn on
  `rhv2`, turn on `ShareConnection` too.**

In a production application the same pair, on a handset over OkHttp, held **2
connections in 1004 of 1004 samples**, with **6 requests in flight over those 2
connections** at the peak; on Windows over netHTTP, 4 in flight over 2 connections.
Every request of the session negotiated h2.

### The one-connection cap (netHTTP only)

WinHTTP will happily open a second connection rather than multiplex onto the first, so
on Windows a shared transport asking for `rhv2` is capped at one connection per server
(`WINHTTP_OPTION_MAX_CONNS_PER_SERVER`), which is what forces multiplexing to actually
happen.

The cap goes on **at transport creation, before the first request** - and that detail
is load-bearing. Capping after the first answer came back left **16 connections where
1 was the point**: 20 threads on a fresh transport all open their socket in the first
burst, before any response exists, and WinHTTP does not close what it already pooled.

Since asking is not getting, the cap comes **off** again the moment an answer reports
`rhv10` or `rhv11` - one burst pays for the wrong guess, and it is right from there on.
`rhvDefault` does not count as a contradiction: it means the transport could not tell,
not that it spoke 1.1.

The cap belongs to the *shared* transport. A client with `ShareConnection = False` gets
a transport of its own and is never capped - which is one reason the `False` + `rhv2`
row above looks the way it does.

---

## The client: KeepAliveInterval

```pascal
property KeepAliveInterval: IntegerRAL read FKeepAliveInterval
  write FKeepAliveInterval default 0;
```

How often, in **milliseconds**, to prove the connection is still there. `0` - the
default - is off, and is what every engine did before.

**Why it exists.** HTTP/2 made the connection long-lived and shared, so a peer that
vanishes - Wi-Fi dropping, a phone changing access point, the server restarting -
leaves no trace at all. TCP does not say, and the client only finds out when
`RequestTimeout` expires. With 60 seconds of read timeout, that is a minute of frozen
screen for a network that died in the first second.

Set, the engine probes the connection on that interval and drops it the moment the
peer does not answer, so every call on it fails in seconds instead.

**Two engines honour it** (`SupportsKeepAliveInterval`), and both send real HTTP/2
PING frames - the 8-byte control frame of RFC 7540 §6.7, answered by the peer's
HTTP/2 layer and never seen by its application:

| | OkHttp, on Android | netHTTP, on Windows |
|---|---|---|
| How | `pingInterval` | `WINHTTP_OPTION_HTTP2_KEEPALIVE` (164), on the session handle |
| When it pings | every interval | after that interval of **inactivity** |
| On a missed pong | fails the connection | not documented; unverified here |
| Minimum | none | **5000 ms** - the option refuses less |
| Availability | the engine has it | **Windows 11 and newer**: measured present on 24H2 build 26100, absent on Windows 10 22H2 build 19045 |

Where the option does not exist, `WinHttpSetOption` returns False and leaves the
session untouched: the request still goes out over HTTP/2, just without a ping.
That is why nothing checks its result to raise.

**The floor belongs to the engine, and it is applied on assignment.**
`MinKeepAliveInterval` answers it - 5000 on netHTTP under Windows, 0 on OkHttp -
and `TRALClient.SetKeepAliveInterval` raises a smaller value to it right there, so
what the Object Inspector shows is what the connection uses. Correcting it inside
the engine instead would leave the screen saying 3000 while the wire used 5000,
which is worse than the limit itself. Changing `EngineType` re-applies the floor,
because the new engine may not be able to keep what the old one could.

Elsewhere the value is ignored, never refused, and the IDE hides the property. It
has no meaning under HTTP/1.1 either, for the same reason it exists: there is no
idle multiplexed connection to probe.

**Picking a value.** OkHttp fails the connection when a pong does not arrive within
the same interval, so detection costs between one and two times the value - 3500 ms
notices a dead peer in 3.5 to 7 seconds. It costs traffic on an idle connection, which
on a handset is battery, so **a value near `ConnectTimeout` is a sensible starting
point, not a small one**:

```pascal
Client.KeepAliveInterval := Client.ConnectTimeout div 2;
```

**It is not an application-level keep-alive.** It does not stop a proxy from closing an
idle connection on a timer of its own, and it is no substitute for a heartbeat request
that keeps a *session* alive. It answers one question only: *is the other end still
there?*

---

## The server: TRALSynopseServer.Mode

```pascal
TRALSynopseMode = (smThreads, smAsync, smHttpSys);
property Mode: TRALSynopseMode read FMode write SetMode default smThreads;
```

| Mode | What runs the sockets | HTTP/2 | An idle connection costs | Platform |
|---|---|---|---|---|
| `smThreads` | a thread per kept-alive connection (mORMot2's socket server) | no | a **thread** | any |
| `smAsync` | mORMot2's event loop - IOCP / epoll | no | a **socket** | any |
| `smHttpSys` | the Windows kernel, http.sys | **yes** | a socket, in the kernel | **Windows only** |

`smThreads` is what the engine always did and stays the default, so nothing changes for
an existing server. Writing `Mode` on a running server restarts it.

`smHttpSys` off Windows raises `emHttpSysWindowsOnly` at start - an explicit choice
that cannot work is refused, not quietly downgraded.

---

## Serving HTTP/2: the http.sys checklist

`smHttpSys` costs a different deployment, and the difference is not cosmetic. Three
things have to be true, none of which RAL can do for you.

**1. The certificate does not come from `SSL.CertificateFile`.** http.sys takes it from
the machine certificate store, bound to the port from outside:

```
netsh http add sslcert ipport=0.0.0.0:9988 certhash=<thumbprint> appid={<any guid>}
```

`SSL.Enabled` still matters - it is what makes RAL listen on https - but the file
properties are ignored. A `.pem` sitting there is not being read.

**2. The URL has to be reserved for the user**, or the process needs Administrator
rights:

```
netsh http add urlacl url=https://+:9988/ user=<user>
```

Without it `AddUrl` fails with **code 5** and RAL raises `emHttpSysAddUrl`, whose
message prints this command with the port already filled in.

**3. `HttpSysDomain` has to match the reservation, literally.**

```pascal
property HttpSysDomain: StringRAL read FHttpSysDomain write FHttpSysDomain;
```

| Value | Accepts |
|---|---|
| `'*'` | any host name that reaches this machine (weak wildcard) |
| `'+'` | every interface, including by IP (strong wildcard) |
| `'localhost'` | loopback only - needs no firewall exception and raises no prompt: the right choice for a local service or a test |

A server asking for `'*'` is **not** covered by a reservation made for `'localhost'`,
and the `AddUrl` comes back with access denied. The other two modes bind a socket
themselves and ignore this property - the IDE hides it for them.

**HTTP/2 itself needs nothing else**: http.sys offers it by ALPN as soon as TLS is
bound. It is turned off machine-wide by `EnableHttp2Tls` in the registry, worth
checking if a correctly deployed server still answers 1.1.

---

## MaxConnections, now the same name on four servers

A ceiling on how many connections may be open **at the same time**. Past it a *new*
connection is refused, so an existing client is never dropped to make room. `0` means
no ceiling.

```pascal
property MaxConnections: IntegerRAL ... default 0;
```

| Server | How it is enforced |
|---|---|
| `TRALIndyServer` | Indy's own `MaxConnections` (`DoMaxConnectionsExceeded`) |
| `TRALSaguiServer` | libmicrohttpd's connection limit |
| `TRALfpHTTPServer` | `OnAllowConnect` + `ConnectionCount` - fcl-web has no setting of its own, but the question it asks before accepting and the count it already holds are exactly this |
| `TRALSynopseServer` | http.sys QoS in `smHttpSys`, `Async.MaxConnections` in `smAsync`. **`smThreads` ignores it** - mORMot2's socket server has no such ceiling there |

**Renamed:** `TRALSaguiServer.ConnectionLimit` is now `MaxConnections`. The old name
survives as a **public, non-published alias**, so existing code compiles unchanged and
the Object Inspector shows the new name only; old `.dfm`/`.lfm` files still load,
through `DefineProperties`, and are never written back with the old name. It carries no
`deprecated` directive because Delphi 12 does not accept one on a property (FPC does,
and a hint on one compiler only is worse than a comment on both).

### Not the same thing: MaxKeepAliveConnections

```pascal
property MaxKeepAliveConnections: IntegerRAL ... default 0;   // TRALSynopseServer only
```

How many clients may hold a **kept-alive** connection at the same time, server-wide.
Only `smThreads` is limited by it, because there the limit is really "how many
threads"; mORMot2's default of 512 was chosen for a 32-bit process, where each thread
reserves stack. On a 64-bit server, raising it costs reserved address space and little
else. `0` keeps mORMot2's default.

**It never refuses a client.** Past the limit the server still answers, but *without*
keep-alive: it closes the socket after each response, and every request of every client
goes back to paying a whole TCP and TLS handshake. That is a latency cliff, and it
arrives silently - which is why the number is published here instead of staying buried
in the engine.

| | `MaxConnections` | `MaxKeepAliveConnections` |
|---|---|---|
| Past the limit | a new connection is **refused** | connections are still accepted, **without keep-alive** |
| Applies to | Indy, fpHTTP, Sagui, mORMot2 (`smAsync` / `smHttpSys`) | mORMot2, `smThreads` only |
| Symptom of a bad value | clients cannot connect | everything works, slowly |

---

## What raises, and what is quietly ignored

One rule, and it is worth internalising:

> **An explicit choice that cannot work raises. A leftover value that does not apply is
> ignored.**

Raises - all of them before a socket is opened, and all with `TransportError` set, so a
caller can tell them apart without matching message text:

| Constant | When |
|---|---|
| `emHTTP2Unsupported` | `HTTPVersion = rhv2` on an engine whose `SupportsHTTP2` is False |
| `emHTTP10NotRequestable` | `HTTPVersion = rhv10` on any engine - nothing can *ask* for HTTP/1.0 |
| `emOkHttpAndroidOnly` | the OkHttp engine sending from anywhere but Android |
| `emHttpSysWindowsOnly` | `Mode = smHttpSys` starting on a non-Windows platform |
| `emHttpSysAddUrl` | http.sys refused the port; code 5 means it is not reserved - the message prints the `netsh` command |
| `emCertPinUnsupported` | `SSL.Pins` has a line for **this host** on an engine whose `SupportsCertPin` is False |

Ignored, never refused:

- `ShareConnection` on an engine that cannot share;
- `KeepAliveInterval` on an engine with no mechanism for it, or under HTTP/1.1;
- `MaxConnections` on `smThreads`, `MaxKeepAliveConnections` on the other two modes,
  `HttpSysDomain` outside `smHttpSys`;
- `rhv2` against a server that has no h2 - the answer simply comes back as `rhv11`,
  which is a successful request.

All the messages live in
[`../languages/ralconsts_enus.inc`](../languages/ralconsts_enus.inc) and its `ptbr` /
`eses` siblings.

---

## The Object Inspector hides what does not apply

A property the current engine or mode cannot act on is **hidden in the IDE**, rather
than shown with a value that does nothing. It works through one virtual method on
`TRALComponent`:

```pascal
function IsPropertyRelevant(const AName: StringRAL): boolean; virtual;   // True
```

Overridden by `TRALClient` (hides `ShareConnection` when the engine cannot share; hides
`KeepAliveInterval` unless the engine supports it **and** `HTTPVersion = rhv2`) and by
`TRALSynopseServer` (hides `HttpSysDomain`, `MaxConnections` and
`MaxKeepAliveConnections` by mode, and the four `SSL` file properties under
`smHttpSys`, where the certificate comes from the machine store and no `.pem` is
ever read). That last one goes one level down, so the component is asked by the
**dotted** name - `IsPropertyRelevant('SSL.CertificateFile')` - and one method
answers for both levels. The filter itself is `TRALSelectionEditor` in
[`../base/RALRegister.pas`](../base/RALRegister.pas), which implements both IDEs'
mechanisms - Delphi's `ISelectionPropertyFilter` and Lazarus's `seaFilterProperties`.

Two things follow:

- **It is cosmetic.** Hidden is not forbidden: a value left over from another
  configuration stays where it is, is ignored, and never raises. Nothing may be built
  on top of `IsPropertyRelevant`.
- **It answers from the class, not from an instance.** At design time there is no
  engine instance to ask, so an `EngineType` the IDE does not recognise shows
  everything - better to show a property than to hide one by accident.

---

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| `Response.ProtocolVersion` is `rhv11` though you asked for `rhv2` | the server has no h2, or the URL is `http://` - ALPN only runs inside TLS |
| ...and the server *does* serve h2 | check it on the **server** side: a Windows client falls back to reading the status line, which h2 does not have |
| `rhv2` set, still one connection per client | `ShareConnection` is False - see the measured table |
| `rhv2` + `ShareConnection`, still many connections | look at the pool key: a different `UserAgent`, timeout, `MaxRedirects` or certificate policy puts a client in its own transport, by design |
| `emHttpSysAddUrl`, code 5 | the URL is not reserved - run the `netsh http add urlacl` the message prints, as Administrator |
| http.sys serves, but always 1.1 | no certificate bound to the port (`netsh http show sslcert`), or `EnableHttp2Tls` is off in the registry |
| the `.pem` in `SSL.CertificateFile` is apparently ignored | it is: under `smHttpSys` the certificate comes from the machine store |
| a dropped Wi-Fi takes `RequestTimeout` to surface on Android | `KeepAliveInterval` is 0 |
| `PROTOCOL_ERROR` from a strict peer | a `Connection` header reaching an h2 request - RAL drops it under `rhv2`, so check anything that adds headers by hand |
| an engine that used to pin now raises `emCertPinUnsupported` | the pin is fine; that engine cannot read a fingerprint on this platform - see the capability table |

---

## Upgrading from an older RAL

Nothing here changes behaviour for code that does not ask for it: `HTTPVersion`
defaults to `rhvDefault`, `ShareConnection` to False, `KeepAliveInterval` to 0, and
`Mode` to `smThreads`. Three things to know anyway:

1. **`TRALSaguiServer.ConnectionLimit` is now `MaxConnections`.** Old code compiles, old
   form files load; only the Object Inspector changed.
2. **`Protocol` on the mORMot2 server used to be a hardcoded `'1.1'`.** It now reports
   what actually arrived, which for an h2 client is `'2.0'`. Anything comparing it to
   `'1.1'` was previously always true and is not any more.
3. **`Clone` now copies `ProtocolVersion`**, so a cloned request or response keeps the
   version it was observed on.

---

## The commits

| SHA | What it did |
|---|---|
| `6ac4eb7` | HTTP/2 on the client, the mORMot2 server modes, and the new OkHttp engine |
| `5d505f7` | ship the OkHttp jars with the engine, and document wiring a client |
| `67ddc3c` | the `OkHttpRAL` design package, so the engine can be picked in the IDE |
| `851fbc8` | docs: HTTP/2, the OkHttp engine and the mORMot2 server modes |
| `cf3f991` | fix the HTTP version every engine reports; unify `MaxConnections` across servers |
| `a6c8560` | fix the one-connection cap arriving too late to do anything |
| `2739f73` | `KeepAliveInterval`, so a dead HTTP/2 peer is noticed in seconds, not minutes |
