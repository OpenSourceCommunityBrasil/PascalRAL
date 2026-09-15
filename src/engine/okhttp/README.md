# OkHttp engine (Android)

`TRALOkHttpClientHTTP` is the only RAL client engine that speaks **HTTP/2 on
Android**, and the only one that can honour `SSL.Pins` there.

## Why it exists

Every other platform already has an engine that negotiates h2: netHTTP reaches
WinHTTP on Windows, NSURLSession on Apple and libcurl on Linux. Android is the
one place where the platform client cannot. `TNetHTTPClient` lands on
`HttpURLConnection`, whose copy of OkHttp AOSP hands a protocol list **without**
h2, so ALPN never offers it — measured against a server proven to serve h2 to
Edge, to a Java 17 client and to WinHTTP, the same application on the handset
always arrived as HTTP/1.1.

The gain is not the speed of one request. It is that requests share a
connection: on a real application with 81 clients, HTTP/1.1 held 5 connections
and h2 holds 2 — one per timeout configuration, which is what `ShareConnection`
asks for.

## What the project needs

Two jars, declared as `<JavaReference>` in the `.dproj`:

| jar | where from |
|---|---|
| `okhttp-4.11.0.jar` | Maven Central, `com.squareup.okhttp3:okhttp:4.11.0` |
| `ralokhttp.jar` | `java/ralokhttp.jar`, next to this file |

What okhttp itself depends on — **okio** and **kotlin-stdlib** — already ships
with the RAD Studio Android runtime (`EnabledSysJars`), so nothing else is
needed.

```xml
<JavaReference Include="java\okhttp-4.11.0.jar">
    <ContainerId>ClassesdexFile</ContainerId>
</JavaReference>
<JavaReference Include="java\ralokhttp.jar">
    <ContainerId>ClassesdexFile</ContainerId>
</JavaReference>
```

Then, in the client:

```pascal
uses
  RALClient, RALOkHttpClient;   // the unit has to be linked in - the engine
                                // is resolved by NAME at runtime

Client.EngineType := ENGINEOKHTTP;
Client.HTTPVersion := rhv2;
```

## The bridge

`java/pascalral/` holds the two classes the engine talks to through JNI:

- **`RalOkHttp`** — one flat, static API: `execute` runs a request and the
  result is read back per thread. Flat because everything crossing JNI costs a
  binding.
- **`RalCertJudge`** — implemented on the Pascal side. It is called **during**
  the TLS handshake, so refusing a certificate aborts the connection before any
  of the request, the token included, has been written. The decision itself
  stays where RAL already makes it, in `AcceptServerCert`, instead of a second
  rule growing inside the bridge.

To rebuild `ralokhttp.jar` after changing them:

```
javac --release 8 -cp okhttp-4.11.0.jar;okio-jvm-3.4.0.jar;kotlin-stdlib-1.8.22.jar ^
      -d classes java/pascalral/*.java
jar --create --file java/ralokhttp.jar -C classes .
```

The okio and kotlin-stdlib jars are in
`$(BDS)\lib\android\debug`.

## Limits

- **Android only.** On any other platform the unit compiles to nothing, and
  netHTTP already does this better.
- **`MaxRedirects` is honoured as on/off.** OkHttp stops at 20 follow-ups and
  does not expose the number, so zero means do not follow and anything else
  means follow.
- **Network calls may not run on the main thread.** Android throws
  `NetworkOnMainThreadException`, exactly as it does for `HttpURLConnection`.
