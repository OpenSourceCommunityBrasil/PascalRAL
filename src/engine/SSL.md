# SSL/TLS in PascalRAL

How to serve **https** with every RAL server engine, how to *call* an https server with
every client engine, and how to make the self-signed certificate each one wants - with
the exact `openssl` command, for each engine, and what to do when there is no
certificate authority in sight.

Companion to [`HTTP2.md`](HTTP2.md), which covers the protocol version. The two meet in
one place: **HTTP/2 only ever happens inside a TLS handshake**, so a server that does not
serve https never serves h2 either.

Everything marked *measured* below was run on 2026-09-16, Windows 10 22H2, Delphi 12,
on loopback: the certificate this page tells you to make, served by a RAL server, called
by a RAL client, with the pin accepting it and a wrong pin refusing it.

---

## Contents

- [The short version](#the-short-version)
- [Three things TLS needs](#three-things-tls-needs)
- [Making a self-signed certificate](#making-a-self-signed-certificate)
- [Which file does my engine want?](#which-file-does-my-engine-want)
- [Server: Indy](#server-indy) *(and the SSLVersions trap)*
- [Server: fpHTTP](#server-fphttp)
- [Server: mORMot2 (smThreads and smAsync)](#server-mormot2-smthreads-and-smasync)
- [Server: mORMot2 (smHttpSys)](#server-mormot2-smhttpsys)
- [Server: Sagui](#server-sagui)
- [Server: CGI and UniGUI](#server-cgi-and-unigui)
- [Client: the three questions](#client-the-three-questions)
- [Client: SSL.Verify](#client-sslverify)
- [Client: SSL.Pins](#client-sslpins)
- [Client: OnValidateServerCert](#client-onvalidateservercert)
- [Client: SSL.Required](#client-sslrequired)
- [What each client engine needs installed](#what-each-client-engine-needs-installed)
- [End to end: a self-signed pair that works](#end-to-end-a-self-signed-pair-that-works)
- [What raises, and what the message means](#what-raises-and-what-the-message-means)
- [Troubleshooting](#troubleshooting)
- [Quick reference](#quick-reference)

---

## The short version

**Make the certificate** (once, in any folder - the command is the same on every OS):

```
openssl req -x509 -newkey rsa:2048 -sha256 -days 3650 -nodes ^
  -keyout key.pem -out cert.pem ^
  -subj "/CN=myserver" ^
  -addext "subjectAltName=DNS:myserver,DNS:localhost,IP:127.0.0.1,IP:192.168.1.10" ^
  -addext "keyUsage=digitalSignature,keyEncipherment" ^
  -addext "extendedKeyUsage=serverAuth"
```

**Server** - pick the two lines for your engine and add `SSL.Enabled := True`:

```pascal
// Indy - the third line is not optional, see the Indy section
RALServer.SSL.SSLOptions.CertFile    := 'cert.pem';
RALServer.SSL.SSLOptions.KeyFile     := 'key.pem';
RALServer.SSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

// fpHTTP
RALServer.SSL.SSLOptions.CertificateFile := 'cert.pem';
RALServer.SSL.SSLOptions.PrivateKeyFile  := 'key.pem';

// mORMot2, smThreads/smAsync, on Windows (SChannel wants ONE .pfx)
RALServer.SSL.CertificateFile := 'cert.pfx';

// mORMot2, smThreads/smAsync, on Linux (OpenSSL wants the two .pem)
RALServer.SSL.CertificateFile := 'cert.pem';
RALServer.SSL.PrivateKeyFile  := 'key.pem';

// Sagui - the CONTENTS, not the file names
RALServer.SSL.Certificate := TFile.ReadAllText('cert.pem');
RALServer.SSL.PrivateKey  := TFile.ReadAllText('key.pem');

RALServer.SSL.Enabled := True;   // every engine, and BEFORE Active := True
RALServer.Active := True;
```

**Client** - the certificate is self-signed, so nothing trusts it. Say which one you
accept, by its fingerprint:

```pascal
RALClient.BaseURL.Text := 'https://192.168.1.10:9988';
RALClient.SSL.Pins.Add('6A:5E:D5:6C:F6:C9:27:CE:1B:AF:D9:B3:CA:98:30:67:' +
                       '1C:17:1B:7D:AB:13:48:94:12:D2:CF:99:87:74:C2:60');
```

That hash comes from:

```
openssl x509 -in cert.pem -noout -fingerprint -sha256
```

Nothing has to be installed on the user's machine, no certificate store is touched, and
a server with a *real* certificate keeps working with no entry at all.

---

## Three things TLS needs

Whatever the engine, https is the same three things, and every difference below is only
about *how each engine is handed them*:

| | What it is | Who has it |
|---|---|---|
| **Certificate** | the server's public identity: its name(s), its public key, and a signature | the server, and it is sent to every client |
| **Private key** | the half that proves the certificate is really yours | the server only - it never leaves the machine |
| **Trust** | a reason for the client to believe the certificate | the client |

A certificate from a public CA (Let's Encrypt, DigiCert...) arrives with the third one
already solved: every machine ships those CAs in its store. **A self-signed certificate
is its own CA**, so nobody trusts it, and the client has to be told - which is the whole
of [`SSL.Pins`](#client-sslpins) further down.

Self-signed is not a lesser kind of encryption. The traffic is encrypted exactly the
same way; what changes is who vouches for the name.

---

## Making a self-signed certificate

You need **openssl**. On Windows it comes with Git for Windows
(`C:\Program Files\Git\usr\bin\openssl.exe`), with any OpenSSL installer, and with
several Delphi components. On Linux and macOS it is already there.

### The one command

```
openssl req -x509 -newkey rsa:2048 -sha256 -days 3650 -nodes ^
  -keyout key.pem -out cert.pem ^
  -subj "/CN=myserver" ^
  -addext "subjectAltName=DNS:myserver,DNS:localhost,IP:127.0.0.1,IP:192.168.1.10" ^
  -addext "keyUsage=digitalSignature,keyEncipherment" ^
  -addext "extendedKeyUsage=serverAuth"
```

(`^` is the line continuation of the Windows prompt. In PowerShell use a backtick
`` ` ``, in bash a backslash `\`, or just write it all on one line.)

Piece by piece:

| Part | What it does | Change it when |
|---|---|---|
| `-x509` | self-signed: no CA is involved, the certificate signs itself | never, for this purpose |
| `-newkey rsa:2048` | makes the private key too, 2048 bits | `rsa:4096` if a policy asks; `ec -pkeyopt ec_paramgen_curve:P-256` for ECDSA |
| `-sha256` | signature hash - SHA-1 is refused by every modern stack | never |
| `-days 3650` | valid for ten years | a shorter life if you rotate |
| `-nodes` | **no password on the private key** - the server reads it with no prompt | see [passwords](#about-the-key-password) below |
| `-keyout` / `-out` | where the key and the certificate go | always |
| `-subj "/CN=..."` | the certificate's name, in the old field | always |
| `-addext "subjectAltName=..."` | **the names that actually count** - see below | always |
| `keyUsage` / `extendedKeyUsage` | marks it as a TLS *server* certificate | never |

### Get the SAN right, or nothing works

`CN` has not been used to match a host name for years. What every client reads is the
**Subject Alternative Name**, and a certificate without one is refused outright by
WinHTTP, SChannel, Android and every browser.

List *every* way a client may address this server, and use the right prefix for each:

```
subjectAltName=DNS:myserver,DNS:myserver.local,IP:192.168.1.10,IP:127.0.0.1
```

- `DNS:` for names - `localhost`, `myserver`, `api.company.local`
- `IP:` for addresses - `192.168.1.10`, `127.0.0.1`, `::1`

An address written as `DNS:192.168.1.10` does **not** work: it has to be `IP:`. This is
the single most common reason a hand-made certificate is rejected.

If your application connects by IP - which is the usual case on a LAN - put the IP in
there. If the machine can change address, that is exactly the situation
[`SSL.Pins`](#client-sslpins) was made for: a pin checks the fingerprint, and then the
name in the certificate stops mattering.

### Check what you made

```
openssl x509 -in cert.pem -noout -text | more
```

Three things worth a look before blaming the code:

```
openssl x509 -in cert.pem -noout -dates          # is it still valid?
openssl x509 -in cert.pem -noout -ext subjectAltName
openssl x509 -in cert.pem -noout -fingerprint -sha256
```

### The fingerprint, which is what the client will pin

```
openssl x509 -in cert.pem -noout -fingerprint -sha256
```

```
sha256 Fingerprint=6A:5E:D5:6C:F6:C9:27:CE:1B:AF:D9:B3:CA:98:30:67:1C:17:1B:7D:AB:13:48:94:12:D2:CF:99:87:74:C2:60
```

Paste it whole into `SSL.Pins` - colons, spaces, lower case, with or without the
`sha256 Fingerprint=` prefix. RAL reduces both sides to hex digits before comparing, so
presentation never decides anything.

### The .pfx, for SChannel and for http.sys

Windows does not read a `.pem` pair. Two RAL configurations need a **PKCS#12** file
instead, which is the certificate and the key wrapped together in one binary:

```
openssl pkcs12 -export -inkey key.pem -in cert.pem -out cert.pfx -passout pass:mypass
```

Use `-passout pass:` (empty) for no password. Those are the two cases:

- **mORMot2 in `smThreads`/`smAsync` on Windows**, where SChannel is the TLS layer -
  see [that section](#server-mormot2-smthreads-and-smasync);
- **`smHttpSys`**, where the `.pfx` is imported into the machine store and bound to the
  port with `netsh` - see [that one](#server-mormot2-smhttpsys).

On a Windows older than 10, if `PFXImportCertStore` refuses the file, export it with the
older ciphers:

```
openssl pkcs12 -export -inkey key.pem -in cert.pem -out cert.pfx -passout pass:mypass ^
  -keypbe PBE-SHA1-3DES -certpbe PBE-SHA1-3DES -macalg sha1
```

### Diffie-Hellman parameters (Sagui only, optional)

```
openssl dhparam -out dh.pem 2048
```

Takes a minute or two. Only the Sagui engine has a property for it, and leaving it empty
is fine on any modern build.

### About the key password

`-nodes` above means "no DES", that is: **the private key file is not encrypted**. It is
the simplest thing that works, and the file's own permissions are what protect it.

To encrypt it instead, drop `-nodes`, and openssl asks for a password. Then fill in the
engine's password property:

| Engine | Property |
|---|---|
| Indy | `SSL.SSLOptions.Key` |
| fpHTTP | `SSL.SSLOptions.KeyPassword` |
| mORMot2 | `SSL.PrivateKeyPassword` (and it is the **.pfx** password under SChannel) |
| Sagui | `SSL.PrivatePassword` |

The password ends up in the `.dfm`/`.lfm` in clear text if you type it in the Object
Inspector. Assign it in code, from wherever your application keeps its secrets.

---

## Which file does my engine want?

The single table to keep at hand. Every server engine also needs `SSL.Enabled := True`,
set **before** `Active := True`.

| Engine | Certificate | Private key | Form | CA / chain |
|---|---|---|---|---|
| **Indy** | `SSL.SSLOptions.CertFile` | `SSL.SSLOptions.KeyFile` | `.pem` **file names** | `SSL.SSLOptions.RootCertFile` |
| **fpHTTP** | `SSL.SSLOptions.CertificateFile` | `SSL.SSLOptions.PrivateKeyFile` | `.pem` **file names** | `SSL.SSLOptions.CertCAFile` |
| **mORMot2** *(Windows, SChannel)* | `SSL.CertificateFile` | *(inside the .pfx)* | **one `.pfx`** | *(ignored)* |
| **mORMot2** *(OpenSSL)* | `SSL.CertificateFile` | `SSL.PrivateKeyFile` | `.pem` **file names** | `SSL.CACertificatesFile` |
| **mORMot2** *(`smHttpSys`)* | - | - | bound by `netsh` | - |
| **Sagui** | `SSL.Certificate` | `SSL.PrivateKey` | the **PEM text itself** | `SSL.Trust` |

Three traps live in that table, and each has cost somebody an afternoon:

1. **Sagui takes the contents, not the path.** `SSL.Certificate := 'cert.pem'` hands the
   literal nine characters to the TLS layer and the server refuses to start.
2. **mORMot2 on Windows takes a `.pfx`, and silently ignores `PrivateKeyFile`.** The
   default TLS layer there is SChannel, not OpenSSL, and SChannel has no concept of a
   separate key file.
3. **fpHTTP takes no file at all, and still serves https** - it makes a certificate up.
   See its section; this is a feature for a first test and a problem in production.

---

## Server: Indy

`TRALIndyServer.SSL` is a `TRALIndySSL`, and everything lives one level down in
`SSLOptions`, which is Indy's own `TIdSSLOptions` plus one property RAL added for the
password.

```pascal
RALServer.SSL.SSLOptions.CertFile     := 'cert.pem';
RALServer.SSL.SSLOptions.KeyFile      := 'key.pem';
RALServer.SSL.SSLOptions.RootCertFile := '';        // only for a chain
RALServer.SSL.SSLOptions.Key          := '';        // key password, if any
RALServer.SSL.SSLOptions.SSLVersions  := [sslvTLSv1_2];
RALServer.SSL.Enabled := True;
RALServer.Active := True;
```

Everything published under `SSLOptions`:

| Property | What it is |
|---|---|
| `CertFile` | the certificate, PEM |
| `KeyFile` | the private key, PEM (may be the same file if you concatenate them) |
| `RootCertFile` | the CA chain, when the certificate is not self-signed |
| `DHParamsFile` | Diffie-Hellman parameters, optional |
| `Key` | **RAL's addition**: the private key's password |
| `Method` / `SSLVersions` | which protocol versions to accept - **see right below** |
| `Mode` | `sslmServer` / `sslmClient` / `sslmUnassigned` - leave it alone, RAL is the server |
| `VerifyMode` / `VerifyDepth` / `VerifyDirs` | *client* certificate checking (mutual TLS) |
| `CipherList` | the OpenSSL cipher string |

### Set SSLVersions, or nothing connects

**Indy's default is `SSLVersions = [sslvTLSv1]` - TLS 1.0 only** (`IdSSLOpenSSL`,
`DEF_SSLVERSIONS`). Windows has been switching TLS 1.0 off for years, so a modern
client and a default Indy server never agree on a version, and the handshake dies
before any certificate is even looked at.

What it looks like is *not* a version error. Measured on Windows 10 22H2, netHTTP
client against this very server:

```
Error sending data: (12175) A security error occurred
```

The text comes from Windows and is in the system's language - what identifies it is
the number. `12175` is `ERROR_WINHTTP_SECURE_FAILURE`, which reads like a certificate
problem and is not one. One line fixes it:

```pascal
RALServer.SSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
```

Leaving 1.0 and 1.1 in the set is harmless - they are offered and never chosen. What
matters is that **1.2 is in there**. Measured: with the default set every call failed
with 12175; with 1.2 added, every one behaved exactly as this document says.

**What has to be on the machine.** Indy in Delphi 12 loads **OpenSSL 1.0.2** by its old
names: `ssleay32.dll` and `libeay32.dll` (the 64-bit builds keep the same names). Put
them next to the executable, or call `IdOpenSSLSetLibPath` before starting the server.
Without them `Active := True` fails with `Could not load SSL library`, and *no* newer
OpenSSL 3 DLL answers to those names - it has to be the 1.0.2 pair.

`SSL.Enabled` decides the whole port: RAL answers `True` to Indy's `OnQuerySSLPort` for
its own port, so one Indy server is either all http or all https, never both.

---

## Server: fpHTTP

`TRALfpHttpServer.SSL.SSLOptions` is fcl-web's `TCertificateData`, with its file-name
properties published.

```pascal
RALServer.SSL.SSLOptions.CertificateFile := 'cert.pem';
RALServer.SSL.SSLOptions.PrivateKeyFile  := 'key.pem';
RALServer.SSL.SSLOptions.KeyPassword     := '';
RALServer.SSL.SSLOptions.HostName        := 'myserver';
RALServer.SSL.Enabled := True;
RALServer.Active := True;
```

| Property | What it is |
|---|---|
| `CertificateFile` | the certificate, PEM |
| `PrivateKeyFile` | the private key, PEM |
| `TrustCertificateFile` | a trusted certificate |
| `PFXFile` | a PKCS#12 instead of the two above |
| `CertCAFile` | the CA chain |
| `KeyPassword` | the private key's password |
| `CipherList` | the OpenSSL cipher string |
| `HostName` | the name - and see right below, because it does more than label |

### The one engine that invents a certificate

If **both** `CertificateFile` and `PFXFile` are empty when the first https connection
arrives, fcl-web generates a certificate and a key in memory for `HostName`, and serves
with those (`TFPCustomHttpServer.CreateSSLSocketHandler`, FPC 3.2.2). The server starts,
answers, and never says a word about it.

That is genuinely useful for a first test - https with zero preparation - and it is the
wrong thing to ship:

- the key is **1024 bits** by default, which modern clients are starting to refuse;
- it carries **no SAN**, so any client that validates names rejects it;
- it is **new on every start**, so its fingerprint changes and a pin cannot be written.

For anything but a scratch test, fill `CertificateFile` and `PrivateKeyFile` in.

**What has to be on the machine.** FPC's OpenSSL units load `libssl`/`libcrypto` by
their platform names - on Windows the `libssl-1_1.dll` / `libcrypto-1_1.dll` (or the
`-x64` pair) next to the executable, on Linux the distribution's own. Without them the
server starts and the handshake fails.

---

## Server: mORMot2 (smThreads and smAsync)

`TRALSynopseServer.SSL` publishes four properties:

```pascal
RALServer.SSL.CertificateFile     := '...';
RALServer.SSL.PrivateKeyFile      := '...';
RALServer.SSL.PrivateKeyPassword  := '';
RALServer.SSL.CACertificatesFile  := '';
RALServer.SSL.Enabled := True;
```

**and what they mean depends on which TLS layer mORMot ends up using.**

### On Windows the layer is SChannel, and it wants a .pfx

mORMot registers SChannel - the Windows TLS stack - by default, and only uses OpenSSL if
the application asks. Under SChannel:

| Property | Under SChannel |
|---|---|
| `CertificateFile` | **a `.pfx` / PKCS#12** holding the certificate **and** the key |
| `PrivateKeyPassword` | the **.pfx** password |
| `PrivateKeyFile` | **ignored** |
| `CACertificatesFile` | **ignored** |

```pascal
RALServer.SSL.CertificateFile    := 'cert.pfx';
RALServer.SSL.PrivateKeyPassword := 'mypass';
RALServer.SSL.Enabled := True;
```

Make that `.pfx` with the [`openssl pkcs12 -export`](#the-pfx-for-schannel-and-for-httpsys)
command above.

Pointing `CertificateFile` at a `.pem` here does not raise anything that names the
real cause: SChannel tries to import it as PKCS#12 and the server stops with

```
TCrtSocket.DoTlsAfter: TLS failed [ESChannel <0.0.0.0>: AfterBind:
PFXImportCertStore returned 80090327 SEC_E_CERT_UNKNOWN,
System Error -2146885630 [CRYPT_E_BAD_ENCODE]]
```

`CRYPT_E_BAD_ENCODE` is Windows saying "this is not a PKCS#12 file", which it is not:
it is a PEM certificate. Measured on Windows 10 22H2. And `PrivateKeyFile` really is
ignored, not merely unused - the same test pointed it at a file that does not exist
and the server started and answered anyway.

There is one more SChannel behaviour worth knowing: **with `CertificateFile` empty it
does not fail - it takes the first certificate with a private key out of the Windows
personal store (`MY`)**. Convenient when the certificate is already installed; confusing
when you expected an error and instead got a certificate you never chose.

### Everywhere else the layer is OpenSSL, and it wants the two .pem

On Linux and macOS, mORMot's OpenSSL unit registers itself at startup, and then:

| Property | Under OpenSSL |
|---|---|
| `CertificateFile` | the certificate, PEM |
| `PrivateKeyFile` | the private key, PEM |
| `PrivateKeyPassword` | the **key's** password |
| `CACertificatesFile` | the CA chain |

### Using OpenSSL on Windows too

If you would rather keep one set of files across platforms, make mORMot prefer OpenSSL
before anything opens a socket:

```pascal
uses mormot.lib.openssl11;
...
OpenSslInitialize;   // once, at program start
```

From that call on, `NewNetTls` points at OpenSSL and the `.pem` columns above apply on
Windows as well. (Defining `FORCE_OPENSSL` for the whole project does the same thing at
compile time.) The OpenSSL 1.1 or 3.x DLLs then have to be next to the executable.

This is also what the *client* side of this engine needs in order to read a fingerprint
at all - see [what each client engine needs](#what-each-client-engine-needs-installed).

### HTTP/2

Neither of these two modes speaks HTTP/2, whatever the certificate. Only `smHttpSys`
does; that is [`HTTP2.md`](HTTP2.md)'s subject.

---

## Server: mORMot2 (smHttpSys)

In `smHttpSys` the sockets belong to the Windows kernel, and **so does the certificate**.
The four `SSL.*` properties are not used - RAL even hides them in the Object Inspector
when `Mode = smHttpSys`, precisely so nobody fills in a file that will never be read.

What is left in RAL is one switch:

```pascal
RALServer.Mode          := smHttpSys;
RALServer.HttpSysDomain := 'localhost';   // or '+' for every host name
RALServer.SSL.Enabled   := True;          // tells http.sys this port is https
RALServer.Active := True;
```

Everything else happens once, outside the program, in an **elevated** prompt.

**1. Put the certificate in the machine store.** From the `.pfx`:

```
certutil -f -p mypass -importpfx My cert.pfx
```

or by hand: `certlm.msc` -> Personal -> Certificates -> All Tasks -> Import.

**2. Find its thumbprint** (SHA-1 here, not SHA-256 - `netsh` still uses SHA-1):

```
openssl x509 -in cert.pem -noout -fingerprint -sha1
```

Strip the colons: `C9C685A3ECE3FCA32A44FB71C17AC9EC11C47C6E`. `certutil -store My` lists
the installed ones in the same form.

**3. Bind it to the port:**

```
netsh http add sslcert ipport=0.0.0.0:9988 certhash=C9C685A3... appid={00112233-4455-6677-8899-AABBCCDDEEFF}
```

`appid` is any GUID you like - it is only a label saying who owns the binding. Generate
one in the IDE with `Ctrl+Shift+G`.

**4. Reserve the URL**, unless the server always runs as administrator:

```
netsh http add urlacl url=https://+:9988/ user=DOMAIN\user
```

**To check, and to undo:**

```
netsh http show sslcert ipport=0.0.0.0:9988
netsh http delete sslcert ipport=0.0.0.0:9988
netsh http delete urlacl url=https://+:9988/
```

If `Active := True` raises `emHttpSysAddUrl` with code **5**, that is access denied: the
URL is not reserved, so run step 4. The full checklist, including what makes http.sys
answer in HTTP/2 rather than 1.1, is in [`HTTP2.md`](HTTP2.md).

---

## Server: Sagui

Sagui is the one engine that takes the **PEM text**, not a file name. It hands the bytes
straight to libsagui's `sg_httpsrv_tls_listen3`.

```pascal
uses System.IOUtils;   // Delphi; on FPC read the file however you prefer
...
RALServer.SSL.Certificate := TFile.ReadAllText('cert.pem');
RALServer.SSL.PrivateKey  := TFile.ReadAllText('key.pem');
RALServer.SSL.Enabled := True;
RALServer.Active := True;
```

| Property | What it is |
|---|---|
| `Certificate` | the **contents** of `cert.pem` |
| `PrivateKey` | the **contents** of `key.pem` |
| `PrivatePassword` | the private key's password |
| `Trust` | the **contents** of a `ca.pem`, for checking *client* certificates |
| `DHParams` | the **contents** of a `dh.pem`, optional |
| `Priorities` | the GnuTLS priority string; default `NORMAL` |

Storing a certificate as text means the Object Inspector will hold the whole PEM block,
newlines and all, inside the `.dfm`. It works, and it is not where a private key belongs:
read the files at runtime, as above.

**What has to be on the machine.** `libsagui-3.dll` on Windows (`libsagui.so.3` on
Linux), from <https://github.com/risoflora/libsagui/releases>, and it has to be a build
**with TLS** - libsagui is compiled against GnuTLS, and the plain builds simply do not
export `sg_httpsrv_tls_listen3`. When they do not, RAL reports
`This version of TLS is not supported by the lib.` through `OnServerError`.

`TRALSaguiServer.LibPath` points at a specific copy if it is not on the search path.

---

## Server: CGI and UniGUI

These two have no `SSL` property, and that is correct: neither of them owns a listening
socket.

- **CGI** runs inside IIS, Apache or nginx. TLS is that server's configuration, and the
  request reaches RAL already decrypted.
- **UniGUI** runs inside the UniGUI server, which has its own TLS settings.

In both cases, from RAL's point of view the connection is plain http, and it should be:
the TLS ended at the front door.

---

## Client: the three questions

Every RAL client answers the same three questions before a byte goes out, and in this
order:

1. **Is https required?** -> `SSL.Required`, and any host with a pin.
2. **Who decides about this certificate?** -> `OnValidateServerCert`, else `SSL.Pins`,
   else the engine.
3. **What does the engine do on its own?** -> `SSL.Verify`.

A plain https URL to a server with a *real* certificate needs none of them: leave
everything at its default and it works. The three exist for the self-signed case, and
for the case of a client that must not be talked down to http.

The decision itself, in one picture:

```
OnValidateServerCert assigned?
        |
       yes -> it decides, alone. Pins and Verify are not consulted.
        |
        no -> does any SSL.Pins line apply to THIS host?
                |
               yes -> ONLY a certificate whose SHA-256 matches one of those
                      lines is accepted - even one the machine store trusts.
                |
                no -> the engine's own verdict, as SSL.Verify configured it.
```

---

## Client: SSL.Verify

`TRALSSLVerify` says what the *engine* should do by itself:

| Value | Meaning |
|---|---|
| `svEngine` | **default** - whatever that engine has always done, which is not the same everywhere |
| `svAlways` | turn validation on where it is off |
| `svNever` | accept any certificate, on every engine |

The default is the honest one and not the safe-sounding one, because the engines
genuinely disagree:

| Client engine | What `svEngine` means there |
|---|---|
| **netHTTP** (Windows) | validates - WinHTTP, against the Windows store |
| **mORMot2** | validates - SChannel or OpenSSL |
| **OkHttp** (Android) | validates - the Android trust store |
| **Indy** | **does not validate at all** (`VerifyMode` empty is `SSL_VERIFY_NONE`) |
| **fpHTTP** | **does not validate at all** (FPC 3.2.2 leaves the chain check out) |

Turning validation on for everybody would have broken plain https on Windows for the two
that do not, because the OpenSSL they load has no certificate store there. So `svEngine`
keeps each one as it was, and `svAlways` is how you ask the other two to check.

**`svNever` is a development tool, not a configuration.** It accepts an expired
certificate, a certificate for another host, and a certificate someone put in the middle
of the connection. If what you actually have is a self-signed certificate you trust, the
answer is a pin - it is no harder to write, and it accepts *that* certificate instead of
all of them.

---

## Client: SSL.Pins

A pin is the answer to "my server has a self-signed certificate and I do not want to
install anything on any machine". You list the certificate's SHA-256, and that is the
only certificate accepted from that host.

```pascal
RALClient.SSL.Pins.Add('6A5ED56CF6C927CE1BAFD9B3CA9830671C171B7DAB13489412D2CF998774C260');
```

### Three shapes of line

```
AB12CD...                    accepted from ANY host
192.168.1.10=CD34EF...       accepted only from that host
10.0.0.7:8443=90FFEE...      only from that host, on that port
[fe80::1]:8443=112233...     IPv6 goes in brackets, so its colons are not a port
```

**Why the left side matters.** One application talks to several servers - some with a
certificate from a public CA, some self-signed. The rule, applied per connection:

- **if any line applies to the host being called**, then *only* a certificate matching
  one of those lines is accepted - even one the machine store trusts;
- **if no line applies**, the certificate is validated as usual.

So the public-CA servers need no entry at all, and nothing breaks the day they renew.
Without the left side, a single bare fingerprint would be claiming to speak for every
server the application calls.

### Rotating a certificate

Several lines for the same host all count. Add the new fingerprint *before* swapping the
certificate on the server, and there is no window in which nothing connects:

```
192.168.1.10=<old fingerprint>
192.168.1.10=<new fingerprint>
```

Remove the old line at leisure.

### What a pin also does

- **It forces https.** A host with a pin refuses a plain `http://` URL - pinning over
  clear text would be checking nothing. The refusal happens before a socket is opened.
- **It replaces the whole verdict.** When a pin decides, the chain, the dates and the
  host name in the certificate stop mattering: the fingerprint is the identity. That is
  what lets one certificate serve a machine whose IP changes.
- **It is checked at assignment.** A line that does not end in 64 hex digits raises
  `emCertPinInvalid` the moment it is added - not at request time, in production.

### Any notation works

All of these are the same pin:

```
6A:5E:D5:6C:...      (openssl)
6a5ed56cf6c927ce...  (lower case, no separators)
6A 5E D5 6C ...      (Windows certificate dialog)
sha256 Fingerprint=6A:5E:...   (pasted whole from the command line)
```

RAL reduces both sides to hex digits once, on assignment, and the engine does the same to
what the server presented.

### Where it works

| Client engine | `SSL.Pins` |
|---|---|
| **Indy** | yes, everywhere |
| **fpHTTP** | yes, everywhere |
| **netHTTP** | yes on **Windows** (read from the WinHTTP handle); no elsewhere |
| **OkHttp** | yes on **Android**; the engine does nothing anywhere else |
| **mORMot2** | yes **with OpenSSL** - under SChannel the certificate never reaches RAL |

A pin on an engine that cannot read fingerprints does not fail quietly: the request
raises `emCertPinUnsupported`, naming the engine, before a socket is opened.

The mORMot2 line has one more failure mode, because it can only be found out after the
handshake: with SChannel (the Windows default) the certificate is never handed over, so
the first request ends with `emCertNotInspectable`. Calling `OpenSslInitialize` at
startup fixes it - see [the next section](#what-each-client-engine-needs-installed).

---

## Client: OnValidateServerCert

When a list of fingerprints is not enough - accept anything from one issuer, check the
dates yourself, log what arrived, ask the user - assign the event. **It is the last
word**: with a handler assigned, neither the pin nor the engine decides.

```pascal
function TForm1.ValidateCert(ASender: TObject;
  const ACert: TRALCertInfo): boolean;
begin
  // ACert.Host and ACert.Port say WHO was being called, so one handler
  // can serve several servers with different policies
  if ACert.Host = '192.168.1.10' then
    Result := ACert.Fingerprint = MY_FINGERPRINT
  else
    Result := ACert.Trusted;
end;
...
RALClient.OnValidateServerCert := ValidateCert;
```

`TRALCertInfo` is the same record on every engine, compiler and platform - which is the
point, since each transport hands its own type to its own callback (`TIdX509` on Indy,
`TCertificate` on netHTTP, `PX509` on OpenSSL) and a check written against any of them
would only work there.

| Field | |
|---|---|
| `Fingerprint` | SHA-256, upper case hex, no separators. **Empty** when the engine cannot produce it |
| `Subject`, `Issuer`, `SerialNumber` | as the engine reported them |
| `NotBefore`, `NotAfter` | validity dates |
| `Trusted` | what the engine's own validation concluded - chain, host, dates |
| `Error` | why not, when `Trusted` is False |
| `Host`, `Port` | who was being called - filled in by RAL, not by the engine |

Fields an engine cannot produce come back **empty, never invented**. Returning `False`
closes the connection before one byte of the request - the token included - has been
sent.

---

## Client: SSL.Required

```pascal
RALClient.SSL.Required := True;
```

Refuses to send anything over plain http, whatever the URL says - so a hand-edited
config file, or a `BaseURL` that failed over to a spare address, cannot silently drop the
connection to clear text. The refusal is `emCertRequiresTLS`, raised before a socket is
opened.

A host with a pin is already required, implicitly.

---

## What each client engine needs installed

| Engine | Platform | TLS comes from | To install | Reads the fingerprint |
|---|---|---|---|---|
| **netHTTP** | Windows | WinHTTP / SChannel | **nothing** | yes |
| **OkHttp** | Android | the Android stack | the two jars (see [okhttp/README.md](okhttp/README.md)) | yes |
| **mORMot2** | Windows | SChannel by default | nothing - but then no fingerprint | only with OpenSSL |
| **mORMot2** | Windows, OpenSSL | OpenSSL | `libssl`/`libcrypto` + `OpenSslInitialize` | yes |
| **mORMot2** | Linux/macOS | OpenSSL, registered on its own | the distribution's OpenSSL | yes |
| **Indy** | all | OpenSSL **1.0.2** | `ssleay32.dll` + `libeay32.dll` | yes |
| **fpHTTP** | all | OpenSSL, through FPC | `libssl-1_1`/`libcrypto-1_1` (or the platform's) | yes |

Two notes that come up often:

**netHTTP asks for nothing, and that is its main argument.** It uses the Windows stack,
against the Windows store, on any machine, with no DLL to deploy - and it still reads the
fingerprint, so a pinned self-signed server needs no installation either.

**mORMot2 on Windows has to be told to use OpenSSL** if you want `SSL.Pins` or
`OnValidateServerCert`:

```pascal
uses mormot.lib.openssl11;
...
OpenSslInitialize;   // once, before the first request
```

Without it the handshake succeeds, the certificate never reaches RAL, and the request
ends with `emCertNotInspectable`. Nothing is silently accepted.

---

## End to end: a self-signed pair that works

A LAN server at `192.168.1.10:9988` and a Windows client, with nothing installed on
either machine.

**1. On the server machine, make the certificate:**

```
openssl req -x509 -newkey rsa:2048 -sha256 -days 3650 -nodes ^
  -keyout key.pem -out cert.pem ^
  -subj "/CN=myserver" ^
  -addext "subjectAltName=DNS:myserver,IP:192.168.1.10,IP:127.0.0.1" ^
  -addext "keyUsage=digitalSignature,keyEncipherment" ^
  -addext "extendedKeyUsage=serverAuth"

openssl x509 -in cert.pem -noout -fingerprint -sha256
```

Write that fingerprint down.

**2. The server** (Indy here; swap the two lines for your engine's, from
[the table](#which-file-does-my-engine-want)):

```pascal
RALServer.Port := 9988;
RALServer.SSL.SSLOptions.CertFile    := 'cert.pem';
RALServer.SSL.SSLOptions.KeyFile     := 'key.pem';
RALServer.SSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
RALServer.SSL.Enabled := True;
RALServer.Active := True;
```

**3. The client:**

```pascal
const
  SERVER_CERT = '6A5ED56CF6C927CE1BAFD9B3CA9830671C171B7DAB13489412D2CF998774C260';

RALClient.EngineType   := 'netHTTP';
RALClient.BaseURL.Text := 'https://192.168.1.10:9988';
RALClient.SSL.Pins.Add('192.168.1.10=' + SERVER_CERT);
RALClient.SSL.Required := True;
```

**What this gets you.** The traffic is encrypted. The client accepts exactly one
certificate from that address and refuses every other, including one a certificate
authority signed. Nothing was installed on the client machine, no store was touched, and
nothing expires in ninety days.

**What it does not get you.** A browser opening the same URL still shows a warning: it
knows nothing about your pin. If browsers have to be happy too, the certificate has to
come from a CA the machine already trusts - a public one, or your own internal CA
installed in the store.

**On the day the certificate is replaced,** add the new fingerprint as a second `Pins`
line, roll out the client, then swap the certificate on the server, then drop the old
line. No outage.

---

## What raises, and what the message means

Every one of these is raised **before a socket is opened**, except the two marked, and
every one also lands in `Response.TransportError` as `rteCertificate` - so code that
already handles transport failures does not need a special case.

| Message | When |
|---|---|
| `emCertPinInvalid` | an `SSL.Pins` line does not end in 64 hex digits - **raised at assignment**, not at request time |
| `emCertRequiresTLS` | `SSL.Required` is set, or a pin applies to this host, and the URL is `http:` |
| `emCertPinUnsupported` | a pin applies to this host and the engine cannot read fingerprints on this platform |
| `emCertNotInspectable` | *(after the handshake)* the engine completed TLS without handing the certificate over - SChannel under mORMot2 |
| `emCertRejected` | *(after the handshake)* the pin or `OnValidateServerCert` refused the certificate |

On the server side, `This version of TLS is not supported by the lib.` reaches
`OnServerError` when libsagui was built without TLS.

Telling a refused certificate apart from a server that is down, in code:

```pascal
if Response.TransportError = rteCertificate then
  // the certificate, not the network
```

---

## Troubleshooting

| Symptom | Cause |
|---|---|
| `Could not load SSL library` on an Indy server | `ssleay32.dll` / `libeay32.dll` missing. It has to be **OpenSSL 1.0.2** - a 3.x DLL renamed does not work |
| Indy server starts, every client fails with `(12175) A security error occurred` | Indy offers **TLS 1.0 only** by default - set `SSL.SSLOptions.SSLVersions` to include `sslvTLSv1_2` |
| mORMot2 server stops at `PFXImportCertStore ... CRYPT_E_BAD_ENCODE` | `CertificateFile` points at a `.pem` and SChannel wants a `.pfx` |
| mORMot2 server on Windows serves a certificate you never configured | `CertificateFile` is empty, so SChannel took the first one from the Windows `MY` store |
| `PrivateKeyFile` is apparently ignored on Windows | it is - under SChannel the key travels inside the `.pfx` |
| Sagui refuses to start with a certificate that is clearly fine | `SSL.Certificate` was given a **file name**; it wants the file's **contents** |
| `This version of TLS is not supported by the lib.` | libsagui was built without TLS - get a TLS build from the releases page |
| fpHTTP serves https although nothing was configured | fcl-web generated a certificate on the fly - see [its section](#server-fphttp) |
| The client refuses with a host-name error | the SAN does not list the name/IP being used, or an IP was written as `DNS:` instead of `IP:` |
| The client accepts a certificate you expected it to refuse | Indy and fpHTTP do not validate under `svEngine` - use `svAlways`, or a pin |
| `emCertPinUnsupported` | that engine cannot read fingerprints there - netHTTP off Windows, OkHttp off Android |
| `emCertNotInspectable` on mORMot2 | SChannel is in charge; call `OpenSslInitialize` at startup |
| `emCertRejected`, and the fingerprint looks right | compare `Fingerprint` against your pin in `OnValidateServerCert` - a pin copied from a `.crt` that is not the server's leaf certificate looks identical to the eye |
| `emCertRequiresTLS` on a URL you believe is https | `BaseURL` may have rotated to a spare address that is still `http:` |
| `emHttpSysAddUrl`, code 5 | the URL is not reserved - `netsh http add urlacl`, as administrator |
| https works, but `ProtocolVersion` is never `rhv2` | that is a different question - [`HTTP2.md`](HTTP2.md) |

---

## Quick reference

**Server**

| | Indy | fpHTTP | mORMot2 | Sagui |
|---|---|---|---|---|
| Enable | `SSL.Enabled := True` | same | same | same |
| Certificate | `SSL.SSLOptions.CertFile` | `SSL.SSLOptions.CertificateFile` | `SSL.CertificateFile` | `SSL.Certificate` |
| Key | `SSL.SSLOptions.KeyFile` | `SSL.SSLOptions.PrivateKeyFile` | `SSL.PrivateKeyFile` *(OpenSSL only)* | `SSL.PrivateKey` |
| Password | `SSL.SSLOptions.Key` | `SSL.SSLOptions.KeyPassword` | `SSL.PrivateKeyPassword` | `SSL.PrivatePassword` |
| CA chain | `SSL.SSLOptions.RootCertFile` | `SSL.SSLOptions.CertCAFile` | `SSL.CACertificatesFile` | `SSL.Trust` |
| Form | file names, PEM | file names, PEM | PEM, or **`.pfx`** under SChannel | **contents**, PEM |

**Client**

| Property | Default | What it does |
|---|---|---|
| `SSL.Verify` | `svEngine` | what the engine does by itself: keep / always / never |
| `SSL.Pins` | empty | the certificates accepted, and where - `HASH`, `host=HASH`, `host:port=HASH` |
| `SSL.Required` | `False` | refuse plain http whatever the URL says |
| `OnValidateServerCert` | unassigned | decide in code; the last word |

**Commands**

```
# certificate + key, self-signed, ten years
openssl req -x509 -newkey rsa:2048 -sha256 -days 3650 -nodes -keyout key.pem -out cert.pem -subj "/CN=myserver" -addext "subjectAltName=DNS:myserver,IP:192.168.1.10"

# the fingerprint for SSL.Pins
openssl x509 -in cert.pem -noout -fingerprint -sha256

# the .pfx for SChannel and for http.sys
openssl pkcs12 -export -inkey key.pem -in cert.pem -out cert.pfx -passout pass:mypass

# what is in there
openssl x509 -in cert.pem -noout -text
openssl x509 -in cert.pem -noout -dates
openssl x509 -in cert.pem -noout -ext subjectAltName

# what a running server is actually serving
openssl s_client -connect 192.168.1.10:9988 -showcerts
```
