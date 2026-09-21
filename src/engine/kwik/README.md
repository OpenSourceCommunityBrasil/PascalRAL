# Kwik engine (Android)

`TRALKwikClientHTTP` is RAL's **QUIC client for Android**, and it speaks the
same wire as `TRALMsQuicClientHTTP`: both build the frame with `RALQuicFrame`,
so one `TRALMsQuicServer` serves a desktop and a handset without knowing which
is on the other end.

## Why it exists

MsQuic is a C library, and on Android that means a `libmsquic.so` built with
the NDK — a platform its own project does not claim and for which nobody
publishes a binary.

And no official Android stack can take its place, because they are all HTTP
clients while this frame wants a **raw bidirectional stream**:

| | speaks QUIC | raw stream | needs a `.so` |
|---|---|---|---|
| OkHttp | no — `Protocol.HTTP_3` is declared with nothing behind it | — | — |
| Cronet / `android.net.http.HttpEngine` | yes, as HTTP/3 | **no** | no |
| Netty incubator QUIC | yes | yes | **yes**, and it ships no Android natives |
| **Kwik** | yes | yes | **no** |

Kwik is QUIC in pure Java: four jars, 646 KB, one set of files whatever the
ABI.

## Putting it in a client, step by step

Everything needed is in `java/`, next to this file.

**1. Copy the five jars into the project.** Any folder will do; the examples
below assume a `java\` folder beside the `.dpr`.

| jar | what it is | licence |
|---|---|---|
| `kwik-0.11.jar` | Kwik itself (`tech.kwik:kwik`) | LGPL v3 |
| `agent15-3.3.jar` | its TLS 1.3 (`tech.kwik:agent15`) | LGPL v3 |
| `hkdf-2.0.0.jar` | `at.favre.lib:hkdf` | Apache 2.0 |
| `io.whitfin.siphash-2.0.1.jar` | `com.io7m.repackage.io.whitfin` | Apache 2.0 |
| `ralkwik.jar` | the bridge this engine talks to, built from `java/pascalral/` | same as RAL |

Nothing there is native, so one set serves every ABI — unlike `libmsquic.so`,
which would be one build per architecture.

**Kwik is LGPL v3**, the first dependency in this repository with a relink
clause. Decide what that means for a closed application before shipping it.

**2. Declare all five in the `.dproj`**, inside the same `<ItemGroup>` that
holds the `<DCCReference>` entries:

```xml
<JavaReference Include="java\kwik-0.11.jar">
    <ContainerId>ClassesdexFile</ContainerId>
</JavaReference>
```

...and the same for the other four. In the IDE the same thing is done through
**Project ▸ Add… ▸ Libraries**.

**3. Add `RALKwikClient` to the `uses`** and pick the engine:

```pascal
Client.EngineType := ENGINEKWIK;
Client.BaseURL.Text := 'https://192.168.1.10:8100';
```

As with every engine, the registration runs from the unit's initialization, so
a unit that is not linked is an engine that does not exist.

## What the platform costs

- **Android 8 / API 26**, from the `java.time` in Kwik's builder.
- agent15 reaches for `XDH` only on the X25519 branch, so secp256r1 — its
  default — keeps the floor off API 33.
- RAD Studio 12 dexes with **D8/R8**, so the Java 11 bytecode of those jars
  goes through. The old `dx` could not have read it.

## Two traps that only show on a device

**agent15 has to be told it is on Android.** It asks the JCA for `RSASSA-PSS`,
which is the JDK's name for it; Android calls the same thing
`SHA256withRSA/PSS` and answers that no such algorithm exists. Every handshake
then fails with

```
Missing RSASSA-PSS support. Did you set
PlatformMapping.usePlatformMapping(PlatformMapping.Platform.Android)?
```

which reads like a warning and is in fact the whole instruction: the mapping
ships inside agent15 and is opt-in. `RalKwik` calls it from a `static` block,
so it happens once per process before any connection can exist. Nothing to do
in the application.

**A pinned certificate would still be refused for its host name.** Kwik's
`customTrustManager()` replaces the trust manager and leaves agent15's
`DefaultHostnameVerifier` running; only `noServerCertificateCheck()` turns both
off, and the builder exposes no way to install a verifier of your own. Since
`src/engine/SSL.md` says a pin or `OnValidateServerCert` makes the host name
stop mattering — which is the normal case for a self-signed certificate reached
by IP — the engine turns both checks off in that mode and judges the chain
itself, **after the handshake and before the first byte of the request**. The
mORMot2 engine settles it the same way, for the same reason.

## What it supports

`SupportsCertPin`, `SupportsSharedConnection` and `SupportsKeepAliveInterval`
are all True. `MinKeepAliveInterval` is 1000 ms because Kwik takes the interval
in whole seconds, so the value read back is the value in effect.

`TRALKwikClientHTTP.DefaultAlpn` is the ALPN offered, `RALQUICALPN` by default
— the same one `TRALMsQuicServer` expects.
