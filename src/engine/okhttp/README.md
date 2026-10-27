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

## Putting it in a client, step by step

Everything needed is in `java/`, next to this file.

**1. Copy the two jars into the project.** Any folder will do; the examples
below assume a `java\` folder beside the `.dpr`.

| jar | what it is |
|---|---|
| `okhttp-4.11.0.jar` | OkHttp itself, from Maven Central (`com.squareup.okhttp3:okhttp:4.11.0`), Apache License 2.0 |
| `ralokhttp.jar` | the bridge this engine talks to, built from `java/pascalral/` |

What OkHttp itself depends on — **okio** and **kotlin-stdlib** — already ships
with the RAD Studio Android runtime (they are in `EnabledSysJars`), so nothing
else has to be added.

**2. Declare both in the `.dproj`**, inside the same `<ItemGroup>` that holds
the `<DCCReference>` entries:

```xml
<JavaReference Include="java\okhttp-4.11.0.jar">
    <ContainerId>ClassesdexFile</ContainerId>
</JavaReference>
<JavaReference Include="java\ralokhttp.jar">
    <ContainerId>ClassesdexFile</ContainerId>
</JavaReference>
```

In the IDE the same thing is done through **Project ▸ Add… ▸ Libraries**. The
build dexes them into `classes2.dex` and the deployment step packages it; both
the IDE and a headless MSBuild honour it.

**3. Point the client at the engine:**

```pascal
uses
  RALClient, RALOkHttpClient;   // the unit has to be linked in - the engine
                                // is resolved by NAME at runtime, so leaving
                                // it out fails only when a request is sent

Client.EngineType := ENGINEOKHTTP;
Client.HTTPVersion := rhv2;
```

**4. Check it took.** `TRALResponse.ProtocolVersion` comes back `rhv2` when ALPN
settled on HTTP/2. If the jars did not make it into the APK the failure is a
`ClassNotFoundException` on the first request, not at build time.

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

Arrays that come back from Java are the caller's to free. A `TJavaArray<T>` is
a plain object, and its destructor is what returns the JNI global reference and
the element copy that `GetByteArrayElements` made; that is why `ReadResponse`
frees the response body in a `finally`. Left behind, every response leaked one
reference and one copy of its body — with a heartbeat every couple of seconds,
a leak that only ended with the process (fixed 2026-09-16).

`ralokhttp.jar` is committed already built, so that using the engine needs no
Java toolchain. After changing the sources it has to be rebuilt, or the jar and
the `.java` beside it drift apart:

```
javac --release 8 -cp okhttp-4.11.0.jar;okio-jvm-3.4.0.jar;kotlin-stdlib-1.8.22.jar ^
      -d classes java/pascalral/*.java
jar --create --file java/ralokhttp.jar -C classes .
```

`okio-jvm` and `kotlin-stdlib` are in `$(BDS)\lib\android\debug`, and `javac`
comes with the JDK that RAD Studio already requires for Android.

## Limits

- **Android only.** On any other platform the unit compiles to nothing, and
  netHTTP already does this better.
- **Delphi only.** There is no Lazarus/FPC package and there cannot be one as
  written: the unit is built on `Androidapi.JNIBridge`, `TJavaGenericImport`,
  `TJavaLocal` and `TJavaArray<T>`, all of them Delphi RTL. FPC would need a
  JNI layer written from scratch.
- **No design-time package yet.** The other engines have one each under
  `pkg/Delphi/Engine/`; this one is reached through the library path. An
  `OkHttpRAL` package would be Android-only, so it is left to the maintainers
  to decide whether it is worth one.
- **`MaxRedirects` is honoured as on/off.** OkHttp stops at 20 follow-ups and
  does not expose the number, so zero means do not follow and anything else
  means follow.
- **Network calls may not run on the main thread.** Android throws
  `NetworkOnMainThreadException`, exactly as it does for `HttpURLConnection`.

## License

OkHttp is © Square, Inc. and is distributed under the Apache License 2.0. The
jar is redistributed here unmodified; the license text is at
<https://github.com/square/okhttp/blob/master/LICENSE.txt>.
