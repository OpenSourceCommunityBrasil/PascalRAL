---
name: ral-debug
description: >
  Finds the cause of an access violation, a hang, a silent process exit or a leak in
  PascalRAL code on Delphi or Free Pascal. Use on "access violation", "runtime error
  217", a server that never stops, a process that disappears under load, a body that
  arrives empty or garbled, or growing memory.
---

# Debugging RAL

Reproduce with the smallest program first: one server, one route, one client call,
outside the repository. Then narrow down.

## FPC: get the real stack

FPC's own crash trace is often garbage (`TExternalThread.Destroy`,
`SysAllocateThreadVars` frames). gdb gives the real frame in seconds:

```bat
gdb.exe -batch -ex run -ex bt <program>.exe
```

`gdb.exe` ships with Lazarus: `mingw\x86_64-win64\bin` in an installer setup, next to
`fpc.exe` in an fpcupdeluxe one (the path is in `ENVIRONMENT.md`). Build with `-g -gl`
for line numbers.

## Leaks

- FPC: build with `-gh` (heaptrc) and read the report at exit. Measure before
  believing any claim about a leak.
- Delphi: `ReportMemoryLeaksOnShutdown := True` in the program, or FastMM in full
  debug mode.
- A transform that raises (a compressor whose DLL is missing, a cipher with a wrong
  key) must still free what it built: look for frees outside `finally`.

## Symptoms that have a known cause

| symptom | look at |
|---|---|
| process disappears under load, no exception | a thread created by a C library with `IsMultiThread` still False (Sagui, MsQuic) |
| AV after a request returns, on a freed form or dataset | a callback still pending: the object must call `Client.DropCallbacks(Self)` in its destructor |
| AV reading a param | `ParamByName` returned `nil`; use `GetQuery`/`GetField`/`GetHeader` or test first |
| server answers 200 with an empty body | an exception swallowed inside the engine; set `OnServerError` and look there |
| body arrives compressed or encrypted | headers read **after** the stream, or `ContentEncoding` read before `TakeWire*`; check the engine's order |
| `Active := False` never returns | a thread blocked in `accept()`/`WaitFor`; the server's destructor must wake it |
| only some requests fail, at random | shared state without a lock, or a check-then-insert in two calls |
| a client sends nothing | params added with `AddValue` (`rpkNONE`) |
| accents become `Ã§` | a non-ASCII literal straight into `StringRAL` on FPC, or a `.inc` saved without BOM |

## Over the wire

- Assert on the **body**, not only the status: several defects answered 200 with the
  wrong body.
- `curl -v` shows exactly what a server sent. A RAL client against a RAL server hides a
  defect present on both sides; test against a third-party peer too.
- Range and overflow checks (`$R+ $Q+`, FPC `-Cr -Co`) turn silent corruption into an
  exception at the line that causes it.
