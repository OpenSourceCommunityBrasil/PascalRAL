---
name: ral-engine
description: >
  Adds or changes a PascalRAL transport engine: a server (TRALServer descendant) or a
  client (TRALClientHTTP descendant) over a networking library, with its package,
  registration and icon. Use for "new engine", "support library X", "port the server to
  Y", or when changing src/engine/<name>.
---

# Write an engine

Engines are **subclasses, not adapters**: `TRALIndyServer`, `TRALSynopseServer`,
`TRALfpHttpServer`... descend from `TRALServer`; clients descend from
`TRALClientHTTP` and are picked at run time by `TRALClient.EngineType`. Read an existing
engine of the same kind end to end before starting: `src/engine/indy` (server and
client, both compilers) is the most complete reference.

## Files

- `src/engine/<name>/RAL<Name>Server.pas`, `RAL<Name>Client.pas`, `RAL<Name>Reg.pas`.
- A binding to a C library goes in `src/bindings/<Lib>.pas` and uses **no** RAL unit
  (`MsQuic.pas`, `libsagui.pas` are shared with other projects).
- Package `<Name>RAL` in `pkg/Delphi/Engine` and `pkg/Lazarus/Engine`, requiring
  `PascalRAL` (`ral-packages`), with its icon (`.rc` line + `Assets/src/_gerardcr.bat`,
  `.lrs`).

## Server: what to override

`SetActive`, `SetPort`, `CreateRALSSL`, `IPv6IsImplemented`, and for TLS
`TLSProvisioning`, `GetTLSCertificate`, `SetTLSCertificate` (so `RALSelfSigned` can hand
it a certificate and swap it under a live listener). `GetServerAddress` only when the
library knows better than the OS.

The request path every server must keep, in this order:

1. Create the `TRALRequest`/`TRALResponse` **inside the callback that holds the
   library's buffer**, and free them there: received bodies are borrowed views, not
   copies.
2. Fill method, URL, headers (`rpkHEADER`), query (`rpkQUERY`), form fields
   (`rpkFIELD`), cookies, `ClientInfo` (IP, port, `ConnectionID`), `ContentEncoding`
   and `ContentEncription` **before** the body.
3. Call `ValidateRequest` (the `ppValidate` plugins) before decoding the body; a status
   >= 400 is answered without decoding.
4. Hand the body over with `SetWireBody(...)` and the right `TRALBodyOwnership`
   (`boBorrowed`, `boBorrowedWritable`, `boOwned`, `boCopy`).
5. Call `ProcessCommands`.
6. Take the answer with `TakeWireStream`/`TakeWireString`, then read `ContentType`,
   `ContentEncoding` and `ContentDisposition` (they reflect what was really done).
7. Call `ARequest.Finish` right after taking the answer (concurrency slots and other
   finish handlers).
8. An exception outside `ProcessCommands` answers 500 through the server's error rules
   (`ErrorText`, `OnServerError`, `RaiseError`), never the library's own page.

Also: `Active := False` must return promptly and free the port (wake any thread
blocked in `accept`); `TCP_NODELAY` on (two sends per answer otherwise stall 40 ms);
a HEAD answers the GET's length with no body; a thread created inside a C library
needs `IsMultiThread := True`.

## Client: what to override

`EngineName`, `EngineVersion`, `PackageDependency` and `SendUrl(AURL, ARequest,
AResponse, AMethod)` (abstract); the capabilities `SupportsCertPin`, `SupportsHTTP2`,
`SupportsHTTPVersion`, `SupportsKeepAliveInterval`, `SupportsSharedConnection` - class
functions answered **outside** any `{$IFDEF}` (the IDE asks them on Windows) - and
`NewCookieJar` when the library has one.

- Transport failures go through `SetTransportError` (`rteConnect`, `rteTimeout`,
  `rteCertificate`, `rteCancelled`, `rteOther`) with `StatusCode` 0. Never resend a
  non-idempotent request after it was written.
- Certificates: translate the library's callback into `TRALCertInfo` and ask
  `AcceptServerCert`; the rule (event, then pin, then the engine's verdict) lives
  there. When a pin or the event decides, the host name stops mattering.
- The `Cookie` header comes from `TRALParams.CookieHeaderText`; the library must not
  write its own.
- `Accept-Encoding` is always sent; `Content-Encoding` only when the body is
  compressed.

## Registration

`RegisterEngine(TRALXxxClientHTTP)` in the client unit's `initialization` and
`UnregisterEngine` in its `finalization`; the server class is registered on the palette
`RAL - Server` by `RAL<Name>Reg`. A platform-specific engine is declared and
registered on every platform and refuses at run time elsewhere (see `RALOkHttpClient`).

## Prove it

Compile on both compilers and every platform it claims (`ral-check-unit`), build the
package (`ral-build`), then run it against the other engines with a real client and a
third-party one (`curl`): assert on the body, not only the status.

Background (Portuguese): wiki
[internos-motores](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/internos-motores).
