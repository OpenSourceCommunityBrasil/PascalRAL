# MsQuic engine (Windows, Linux, Android)

`TRALMsQuicServer` and `TRALMsQuicClientHTTP` carry RAL over **QUIC**, through
Microsoft's [msquic](https://github.com/microsoft/msquic) library: one QUIC
stream per request, so a lost packet delays only the request it belongs to, a
1-RTT handshake, and a connection that survives the client changing network.

What travels is **not HTTP/3**: msquic implements the transport only (streams,
TLS 1.3, ALPN), and RAL puts its own length-prefixed frame on each stream. Both
ends must therefore be RAL - curl, a browser, a reverse proxy or a CDN cannot
read it. `TRALKwikClientHTTP` speaks the same frame, so one server serves both
clients.

## Platforms

| Platform | Server | Client | Library | Verified |
|---|---|---|---|---|
| Windows x64 | yes | yes | `msquic.dll` | Delphi and FPC, on a running pair |
| Windows x86 | yes | yes | `msquic.dll` (x86) | Delphi, on a running pair |
| Android arm64-v8a | yes | yes | `libmsquic.so` | on a handset, Android 16 |
| Android armeabi-v7a | yes | yes | `libmsquic.so` | on a handset, Android 16 |
| Linux x64 | yes | yes | `libmsquic.so.2` | compiles; not run here |
| macOS | - | - | - | refused at load: the status codes there are not translated |

## Getting the library

It has to be the **OpenSSL** build of msquic. On Windows that is not a detail:
the SChannel build relies on the system's TLS 1.3, which only exists from
Windows 11 and Server 2022, and QUIC does not exist without it.

The four binaries RAL is verified with are in the repository's **`external`
branch**, under `msquic/`, one zip per platform with the binary and its
licences:

| Zip | Inside |
|---|---|
| `msquic-2.6.1-windows_x64.zip` | `msquic.dll`, from Microsoft's NuGet package `Microsoft.Native.Quic.MsQuic.OpenSSL` |
| `msquic-2.6.1-windows_x86.zip` | the same, 32-bit |
| `msquic-2.6.1-android_arm64-v8a.zip` | `libmsquic.so`, built from the v2.6.1 source |
| `msquic-2.6.1-android_armeabi-v7a.zip` | the same, 32-bit |

msquic does not publish Android binaries, so the two `.so` were built from
source; the `README.md` beside the zips has their SHA-256 and the recipe that
rebuilds them.

## Windows

Put the `msquic.dll` of the right bitness beside the executable - the x86 one
for a 32-bit program. `LibPath` on the server, or
`TRALMsQuicClientHTTP.DefaultLibPath` for the client, points somewhere else.

## Linux

`libmsquic.so.2`, the OpenSSL build, from Microsoft's Linux packages or built
from source. The client validates certificates with OpenSSL against its default
paths - `SSL_CERT_FILE` and `SSL_CERT_DIR` redirect them - or against
`TRALMsQuicClientHTTP.DefaultCaFile`.

## Android, step by step (Delphi)

The same two units run there unchanged, server included.

**1. Put the two libraries in the project**, for example as
`lib\arm64-v8a\libmsquic.so` and `lib\armeabi-v7a\libmsquic.so` beside the
`.dpr`.

**2. Add them to the deployment** (Project ▸ Deployment), with these remote
paths:

| Platform | File | Remote path |
|---|---|---|
| Android 64-bit | `lib\arm64-v8a\libmsquic.so` | `library\lib\arm64-v8a\` |
| Android 32-bit | `lib\armeabi-v7a\libmsquic.so` | `library\lib\armeabi-v7a\` |
| Android 64-bit, with the 32-bit slice | both of the above | both of the above |

In the `.dproj` the same thing reads:

```xml
<DeployFile LocalName="lib\arm64-v8a\libmsquic.so" Configuration="Release" Class="File">
    <Platform Name="Android64">
        <RemoteDir>library\lib\arm64-v8a\</RemoteDir>
        <RemoteName>libmsquic.so</RemoteName>
        <Overwrite>true</Overwrite>
    </Platform>
</DeployFile>
```

...and the same for `Debug`, and for the 32-bit file under `Android`.

**3. Leave `LibPath` and `DefaultLibPath` empty.** The folder those files land
in is the application's own, which is where the dynamic linker finds a plain
`libmsquic.so`.

**4. Link the unit and pick the engine:**

```pascal
uses
  RALClient, RALMsQuicClient;   // the engine is found by NAME at runtime:
                                // a unit left out fails only when a request
                                // is sent
...
Client.EngineType := ENGINEMSQUIC;
Client.BaseURL.Text := 'https://192.168.1.10:8100';
Client.SSL.Pins.Add('AB:CD:...');   // a self-signed server reached by IP
```

**5. For a server on the handset**, deploy the certificate and the key with the
remote path `assets\internal\` - `System.StartUpCopy` copies that folder to
`TPath.GetDocumentsPath` on the first run - and point the server there:

```pascal
Server.SSL.CertificateFile := TPath.Combine(TPath.GetDocumentsPath, 'cert.pem');
Server.SSL.PrivateKeyFile  := TPath.Combine(TPath.GetDocumentsPath, 'key.pem');
Server.Active := True;
```

### What Android costs

- **Android 9 (API 28) or newer.** msquic's OpenSSL glue calls `glob()`, which
  the NDK only declares from API 28, so the library is built against it - and
  the symbol versions that build records (`getentropy@LIBC_P`) make the dynamic
  linker refuse it on Android 8. The engine then fails its first request, or
  `Active := True`, with the linker's own message. Set `minSdkVersion=28` to keep
  the application off those devices, or use `TRALKwikClientHTTP`, which runs
  from Android 8, for the client.
- **3.5 to 3.9 MB per ABI**, OpenSSL included.
- The `.so` are aligned to **16 KB pages**, which Android 15 and later require
  of 64-bit libraries.

## Certificates

A server always needs its two `.pem` files: QUIC has no plain mode.

A client decides about the server's certificate the way every RAL client does -
`OnValidateServerCert`, else `SSL.Pins`, else the engine's own validation (see
[../SSL.md](../SSL.md)). What "the engine's own validation" is depends on the
platform:

| Platform | Validated against |
|---|---|
| Windows | the Windows certificate store |
| Android | the system's trusted CAs, which the engine gathers into one file in the application's cache folder, once per process |
| Linux | OpenSSL's default paths |

`TRALMsQuicClientHTTP.DefaultCaFile` replaces that store with a PEM bundle of
your choosing, on every platform - the way to trust a private CA without
installing it. Set it before the first request.

For a self-signed certificate reached by IP the answer is a pin, not a CA:
`SSL.Pins` accepts that one certificate, and only it.

## Settings worth knowing on a handset

- **`KeepAliveInterval`** (client and server): a QUIC PING on a quiet connection,
  so a NAT does not forget its mapping. Zero, the default, sends nothing.
- **`Migration`** (server, on by default): the connection survives the client
  walking from Wi-Fi to mobile data.
- **`ShareConnection`** (client, on by default): every client aimed at the same
  server shares one connection, each request on its own stream.

## What was verified, and how

On 02/10/2026, with the binaries above:

- a functional program of 33 cases - gzip, AES-256, multipart, cookies, the
  caller's address over IPv4 and IPv6, a server that is down, pins, the event,
  a CA file, the 413 ceiling, a 3 MB answer, `PoolCount`, restarting - on
  Delphi Win32 and Win64, on FPC x64, and on the handset in both ABIs with the
  server and the client on the device;
- a Windows client against the handset's server over Wi-Fi, in both ABIs;
- a public CA chain validated through the Android store, and refused under an
  unrelated CA file;
- the record layout of `MsQuic.pas` against `msquic.h` for ARM32 and ARM64,
  field by field.

Not verified here: Linux at runtime, and an Android client against a Windows
server.

## Licences

msquic is **MIT**. The OpenSSL inside the Android libraries and the quictls
inside the Windows ones are **Apache 2.0**. Both texts are inside every zip, and
both have to travel with an application that ships the library.
