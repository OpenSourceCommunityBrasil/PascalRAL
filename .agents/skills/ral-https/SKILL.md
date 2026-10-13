---
name: ral-https
description: >
  Sets up HTTPS with PascalRAL: certificates on each server engine, the automatic
  self-signed certificate plugin (TRALSelfSignedPlugin), and on the client which
  certificates to accept (SSL.Pins, SSL.Required, SSL.Verify, OnValidateServerCert).
  Use for "enable HTTPS", "self-signed certificate", "certificate pinning", "accept
  only my server", or errors such as 12175, rteCertificate or a refused handshake.
---

# HTTPS

| situation | path |
|---|---|
| HTTPS with nothing prepared | `TRALSelfSignedPlugin` (package `RALPluginTLSPackage`) |
| I have the certificate files | configure the engine (below) |
| public server with a domain | a reverse proxy (nginx) in front, RAL on plain http behind it |
| the client must trust a self-signed server | `SSL.Pins` on the client |

## Automatic self-signed certificate

```pascal
uses RALSelfSigned;

RALSelfSignedPlugin1.Server := Server;   // or Server.AddPlugin(...)
Server.Active := True;                   // comes up on https
ShowMessage(RALSelfSignedPlugin1.Fingerprint);   // what clients pin
```

It leaves a CA-issued certificate alone, creates or reuses one in `Folder` otherwise,
and renews it `RenewBeforeDays` before expiry without stopping the server. Settings:
`HostNames` (**add the machine's LAN address**), `ValidDays`, `RenewBeforeDays`,
`AutoRenew`/`CheckInterval`, `Folder` (never next to an exe that serves files),
`FileName`, `KeyBits`. A renewal changes the fingerprint: `OnCertificateRenewed`, and
pin both during the change. Pure Pascal: no OpenSSL needed to generate it; the engine
still needs what it always needs for TLS.

## Certificate files, per engine

| engine | what it takes |
|---|---|
| Indy | `SSL.SSLOptions.CertFile`/`KeyFile` and **`SSLVersions` including `sslvTLSv1_2`** (without it every client fails with `12175`), OpenSSL 1.0.2 DLLs; `SSL.Enabled := True` before `Active` |
| fpHTTP | `SSL.SSLOptions.CertificateFile`/`PrivateKeyFile`; without files it invents a weak certificate per start |
| mORMot2 sockets | Windows: a `.pfx` in `SSL.CertificateFile` + `SSL.PrivateKeyPassword`; Linux: the two `.pem` |
| mORMot2 `smHttpSys` | the Windows certificate store, bound with `netsh http add sslcert`; HTTP/2 |
| MsQuic | always TLS 1.3; `msquic.dll` from the **OpenSSL** build |
| CGI, UniGUI | the front server's TLS |

## The client: which certificate to accept

With nothing set, each engine does what it always did (netHTTP and mORMot2 validate;
Indy and fpHTTP do not verify at all).

| member | does |
|---|---|
| `SSL.Pins` | accepted SHA-256 fingerprints, one per line: `fp` (any host), `host=fp`, `host:port=fp`, `[ipv6]:port=fp`. A pinned host accepts only those, ignores the name, and refuses plain http |
| `SSL.Required` | refuses any plain `http://` |
| `SSL.Verify` | `svEngine` (default), `svAlways`, `svNever` - used when there is no pin and no event |
| `OnValidateServerCert` | your code decides (`TRALCertInfo`: `Fingerprint`, `Subject`, `Issuer`, dates, `Trusted`, `Host`, `Port`) |

```pascal
Client.BaseURL.Text := 'https://192.168.0.11:9988';
Client.SSL.Pins.Add('6542DB04A4778A33BDBDF9EC5C953793D4F73572EA804C145FBB55641735290A');
```

- Rule: the event decides, else the pin, else the engine's verdict.
- Engines that can pin (`SupportsCertPin`): Indy, netHTTP on Windows, OkHttp on Android,
  MsQuic. Pins on another engine raise on the first request instead of checking less.
- A refusal is `TransportError = rteCertificate`, and happens before any byte (token
  included) is sent. Two lines for one host let you rotate a certificate.
- Get a fingerprint: `openssl x509 -in cert.pem -noout -fingerprint -sha256`.

Guides (Portuguese): wiki
[https](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/https),
[https-cliente-ral](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/https-cliente-ral),
[certificado-automatico](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/certificado-automatico);
engine details in `src/engine/SSL.md`.
