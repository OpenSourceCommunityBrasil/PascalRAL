# Mail in PascalRAL

`RALModuleMailPackage` writes and reads e-mail messages, sends them over SMTP, keeps a
persistent send queue, signs and checks DKIM, and receives mail with an SMTP server. It
needs no third-party library except OpenSSL for TLS, loaded at run time. This file is
the map for whoever changes it; the user guide is the wiki (`email*.md` pages).

## Packages

| package | holds | requires |
|---|---|---|
| `RALModuleMailPackage` (runtime) | every unit of this folder | `PascalRAL`, `RALNetPackage`, `RALCryptoCurvesPackage`, `RALHostingJobQueuePackage` |
| `RALModuleMailDsgn` (design) | `design/RALModuleMailReg`: palette, "Test connection" (client), "Test ports" (server) | `RALModuleMailPackage`, `designide` |

`RALCryptoCurvesPackage` gives Ed25519 to DKIM; `RALHostingJobQueuePackage` gives the
queue its store, workers and retries.

## Units

| unit | what | standards |
|---|---|---|
| `RALMailMIME` | `TRALMailMessage`, addresses, attachments; writing and reading as streams; encoded words, parameters, quoted-printable, dates | RFC 5322, 2045-2049, 2047, 2231, 2392, 6532 |
| `RALSMTPClient` | `TRALSMTPClient`: security modes, SASL, PIPELINING, CHUNKING, SIZE, 8BITMIME, SMTPUTF8 | RFC 5321, 3207, 4954, 2920, 3030, 1870, 6152, 6531, 7677, 7628 |
| `RALDKIM` | `TRALDKIMSigner`, `TRALDKIMVerifier`, canonicalization | RFC 6376, 8463 |
| `RALMailQueue` | `TRALMailQueue` on a `TRALJobQueue` | RFC 5321 4.5.4 (retry) |
| `RALSMTPServer` | `TRALSMTPServer` on `TRALSocketLoop`, the connection state machine | RFC 5321, 3207, 4954, 2920, 3030, 1870, 2034, 3848 |

## Things that are easy to break

- **Bytes, not strings.** Wire text is `RawByteString`. Delphi's `Trim` and
  `UniqueString` have no `RawByteString` version and convert through `string`: the units
  carry their own (`TrimRaw`, `TrimWSP`, `LowerAscii`).
- **The client's pipeline.** MAIL, every RCPT and (without CHUNKING) DATA go in one write;
  the replies are read in that order even when MAIL failed, or the next command reads a
  stale reply. When no recipient is accepted the exception handler sends the RSET: do
  not add one before raising.
- **DKIM in `SendRaw`.** The signature covers the bytes on the wire, so it is computed on
  a signed copy inside `SendRaw` (which then calls itself without the signer), not in
  `Send`. Ed25519-SHA256 signs the SHA-256 of the canonicalized data (RFC 8463 3), not
  the data. The RFC 8463 appendix message must verify with both keys.
- **The queue counts recipients.** A recipient the server took is never sent again:
  when only some fail, a new job carries the rest and the attempts already made (the
  job's own `Attempts` restarts at 1). Delivery is at least once: a process killed
  between the final 250 and the job's completion sends that message again.
- **The server's buffer.** `Serve` reads a 16 KB block and serves it before reading the
  next; reading all that is available first lets a fast client fill memory with a big
  message.
- **The end of DATA** is only `CRLF.CRLF`: a line starts only after CRLF, so a bare LF
  followed by a dot is content (SMTP smuggling).
- **STARTTLS.** Plain text the client sent after the command is dropped before the
  handshake (CVE-2011-0411), and the session starts over (no EHLO, no AUTH).
- **Ownership.** `TRALSocketChannel` frees its socket and `TRALTLSChannel` its inner
  channel: a connection frees only the top of the stack.
- **Events run on the server's poll threads.** A slow `OnMessage` holds every
  connection of its thread.

## Tests

Local tests (not in the repository) cover the client against a test server, DKIM against
the RFC 8463 vectors, the queue killed in the middle of a batch, and the server against
the RAL client, Python's smtplib and raw sockets (pipelining, smuggling, STARTTLS
injection, 1000 connections, a 100 MB message with constant memory).
