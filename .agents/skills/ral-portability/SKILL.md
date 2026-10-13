---
name: ral-portability
description: >
  Writes PascalRAL code that compiles and behaves the same on Delphi (XE and later) and
  Free Pascal/Lazarus, on Windows, Linux, macOS and Android. Use when writing or
  reviewing any unit in src/, on an error that appears on one compiler or platform
  only, or around IFDEFs, strings, atomics, includes and encodings.
---

# Delphi and FPC, every platform

RAL compiles one `src/` tree with Delphi XE and later and with FPC 3.2 (Lazarus), for
Windows, Linux, macOS and Android. These are the differences that have bitten.

## Conditionals

- `src/core/PascalRAL.inc` is the **only** place compiler, OS and framework symbols are
  defined. Use its symbols (`DELPHIXE7UP`, `RALWindows`, `RALLinuxFPC`,
  `NewDelphiAndLazarus`, `HAS_FMX`, `CPU64`...), never `CompilerVersion` or `VERxxx`.
  Include it by relative path: `{$I ..\core\PascalRAL.inc}`.
- An `{$IFDEF}` wraps a routine's **declaration and its body together**. A body inside
  `{$IFDEF RALWindows}` with the declaration outside compiles on Windows only.
- Test what a library **has**, not its version: `{$IF DECLARED(TSomething)}`. FPC trunk
  is 3.3.1 across years of different APIs.
- A platform-specific class is still declared and registered on every platform, and
  refuses at run time elsewhere; its `Supports*` class functions answer what the class
  does, outside the `{$IFDEF}` (the IDE asks them on Windows).

## Language differences

| on | do |
|---|---|
| method pointers on FPC | `{$IFDEF FPC}@{$ENDIF}Handler` |
| `AtomicIncrement`/`AtomicDecrement` (Delphi only) | `RALAtomicInc`/`RALAtomicDec` (`RALTools`), which return the new value on both |
| `Windows` and `SyncObjs` in one `uses` | `Windows` first: FPC's `Windows` declares a record `TCriticalSection` that shadows the class |
| parameterless procedural variables | FPC (ObjFPC) does not call them without `()`; such units use `{$MODE DELPHI}` |
| `finalization` with no `initialization` | Delphi refuses it (E2029): write an empty `initialization` |
| `FillChar` over a record with strings | `Finalize` it first, or use `Default(T)` where available |
| `x[0]` of an empty array or string | raises under `$R+`; guard by length |
| enum ordinal from outside | Delphi compares small enums as signed bytes: check as `Cardinal` |

Applications build RAL with `$R+ $Q+` (FPC `-Cr -Co`): code must not rely on them being
off, except hashing and ciphers, which turn them off locally.

## Strings

- `StringRAL` is `UTF8String` on both compilers (`RALTypes`); use the RAL aliases
  (`StringRAL`, `CharRAL`, `IntegerRAL`, `Int64RAL`, `PCharRAL`) in public signatures.
- **Delphi's RTL is UTF-16**: `SameText`, `Trim`, `LowerCase`, `StringReplace`,
  `GetEnumName` over a `StringRAL` convert both ways (warning `W1057`). On hot paths use
  `RALSameName`, `RALTrim`, `RALPosText`.
- **FPC with `LazUTF8`** (pulled in by `RALTypes`) re-encodes an untyped non-ASCII
  literal passed straight to a `StringRAL`: `'ç'` becomes `Ã§`. Go through a typed
  constant, or declare `{$codepage utf8}`.
- **Never** keep raw bytes (ciphertext, digests) in a `StringRAL`: the code page
  conversion rewrites them. Use `TBytes` or a stream.
- `AStream.WriteBuffer(S[1], ...)` on a shared string makes Delphi copy it first;
  `Pointer(S)^` does not.

## Files and encodings

- `src/languages/ralconsts_*.inc` are **UTF-8 with BOM**, and `RALConsts.pas` declares
  `{$CODEPAGE UTF8}` under FPC; keep both, and keep `RALConsts.pas` ASCII. Edit those
  files byte-exact: a tool that re-encodes them turns the accents into mojibake.
- A user-facing string goes into all three language files.
- New comments are ASCII.

## Threads

- A thread created inside a C library (Sagui, MsQuic callbacks) never runs
  `BeginThread`: set `IsMultiThread := True` before handing it Pascal code, or the
  memory manager does not lock.
- FPC on Linux needs `cthreads` first in the program's `uses`.

## Verify

Compile the touched units on every platform the package claims: `ral-check-unit`.
