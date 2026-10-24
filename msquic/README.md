# msquic 2.6.1 - the binaries RAL's MsQuic engine is verified with

One zip per platform; each opens into a folder named after it, holding the
library and the licences that have to travel with it. Where each file goes in
an application, and what each platform costs, is in the main branch:
`src/engine/msquic/README.md`.

| Zip | Library | SHA-256 of the library |
|---|---|---|
| `msquic-2.6.1-windows_x64.zip` | `msquic.dll` (4.2 MB) | `60180e69a9c285600c80ade43c56d6d4f0a04f7a78074f8d00fcc205866d975e` |
| `msquic-2.6.1-windows_x86.zip` | `msquic.dll` (2.8 MB) | `8e3a35570906a154b5d14353bfcba488610019aacc772d3f46af4767a8285654` |
| `msquic-2.6.1-android_arm64-v8a.zip` | `libmsquic.so` (3.9 MB) | `e05fa6dd6709fbfe72698c381cb71f0b56a779d427391b0aa11e611615a8ba67` |
| `msquic-2.6.1-android_armeabi-v7a.zip` | `libmsquic.so` (3.5 MB) | `e5d647f5bbbe159041c2d2edad672958feaa434b5355af60e6f1f731034654d9` |

All four are the **OpenSSL** build of msquic. On Windows that is what makes QUIC
work at all on Windows 10: the SChannel build depends on the system's TLS 1.3,
which only exists from Windows 11 and Server 2022.

## Windows: from Microsoft's own package

Both DLLs are taken unchanged from the official NuGet package
`Microsoft.Native.Quic.MsQuic.OpenSSL` 2.6.1 (`build/native/bin/x64` and
`bin/x86`), version `2.6.1.156129559-official`, Authenticode-signed by
Microsoft Corporation. quictls is linked statically inside them.

## Android: built from source

msquic publishes no Android binary, so these two are built from:

- msquic, tag `v2.6.1` (commit `a01333cf7c2659cce0ff03ef3f21e1ff15bb5b83`);
- OpenSSL 3.5.8-dev, the commit msquic's submodule points at
  (`453eaaa9e6bb1304730abacfbb73d51868cb6ab9`), linked statically.

Properties worth knowing, all read off the files themselves:

- **Android 9 (API 28) or newer.** msquic's `selfsign_openssl.c` calls `glob()`,
  which the NDK only declares from API 28, so the library is built against it;
  the symbol versions that link records (`getentropy@LIBC_P`) then make the
  dynamic linker refuse the library on Android 8 and older.
- Every `LOAD` segment is aligned to **16 KB**, which Android 15 and later
  require of 64-bit libraries (`-Wl,-z,max-page-size=16384`).
- Stripped (`llvm-strip --strip-unneeded`); the only dependencies are `libc`,
  `libm` and `libdl`; the exports are `MsQuicOpenVersion` and `MsQuicClose`.

### Rebuilding them

`android-build/` holds the recipe, which needs nothing installed beyond RAD
Studio 12 (its Android SDK brings CMake 3.22 and Ninja, its NDK r21 the
compilers) and Git for Windows (bash and perl):

1. Work in a **short path**, `C:\msq`: under a deep folder CMake creates files
   past 260 characters and loops ("`build.ninja` still dirty after 100 tries").
   Copy this folder's content there.
2. Fetch msquic `v2.6.1` into `C:\msq\msquic`, and OpenSSL at the commit above
   into `C:\msq\msquic\submodules\openssl` (a shallow `git submodule` fetch
   fails; the commit's `.tar.gz` from codeload.github.com works).
3. `bash openssl.sh arm64-v8a`, then `build.cmd arm64-v8a` and
   `build.cmd arm64-v8a build`. The library lands in
   `C:\msq\out-arm64-v8a\ready`. The same with `armeabi-v7a`.
4. Check the alignment with `llvm-readelf -l libmsquic.so`: every `LOAD` must
   say `Align 0x4000`.

Why the OpenSSL build is separate, and what the `perl5` folder is: msquic's
embedded OpenSSL build uses Unix shell syntax that Ninja on Windows hands to
`cmd.exe`, and Git's perl lacks three modules OpenSSL's `Configure` loads only
to format messages and find executables - `perl5/` has minimal stand-ins. The
comments at the top of each script carry the rest.

## Licences

- **msquic**: MIT License, Copyright (c) Microsoft Corporation -
  `LICENSE-msquic.txt` in every zip.
- **quictls** (inside the Windows DLLs) and **OpenSSL** (inside the Android
  libraries): Apache License 2.0 - `LICENSE-quictls.txt` / `LICENSE-openssl.txt`.

Both have to accompany an application that ships the library.
