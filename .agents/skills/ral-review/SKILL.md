---
name: ral-review
description: >
  Reviews a change to PascalRAL before it is handed over: the invariants the library
  relies on (public API, body ownership, engines, threads, secrets, published defaults,
  registration) and the shape of a proposal. Use before committing, before opening a
  pull request, or when asked to review a diff in src/.
---

# Review a change

Read the diff against each item. A "no" is a finding; say which line and why.

## Scope and API

- [ ] Anything in `src/core` changed? Its public surface (`TRALServer`, `TRALRequest`,
      `TRALResponse`, `TRALParams`, `TRALClient`) is used by every engine, every module
      and downstream code: check the engine subclasses (`src/engine/*`) and the
      `TRALModuleRoutes` descendants (`src/database`, `src/modules/*`).
- [ ] No public member renamed or removed without a `deprecated` path. A breaking change
      is said in the commit subject (`ral-commit`).
- [ ] Defaults keep the old behaviour. A new limit or protection is **off** by
      default; the application turns it on.
- [ ] RAD first: a new feature is configurable in the Object Inspector
      (`ral-rad-component`).

## Correctness traps of this code base

- [ ] **Published `default` equals what the constructor sets.** Streaming skips a value
      equal to `default`, so a mismatch silently changes forms. Both sides read one
      constant (`RALConsts`).
- [ ] **Secrets are compared with `RALSameSecret`** (or `RALSameBytes`), never `=`.
- [ ] **Body ownership** (`TRALBodyOwnership`): whoever hands a stream to the params says
      whose it is (`boBorrowed`, `boBorrowedWritable`, `boOwned`, `boCopy`). A received
      body is a read-only view; `TRALParam.Content` is not always a `TMemoryStream`.
- [ ] **Engines**: the request is created and freed inside the callback that holds the
      buffer; `TRALRequest.Finish` is called right after `TakeWireStream`;
      `ContentType`/`ContentEncoding` are read after `TakeWire*`.
- [ ] **Errors**: test `ErrorCode <> 0` for a transport failure, never `StatusCode`. A
      client never resends a non-idempotent request after it was written.
- [ ] **`AddValue` gives `rpkNONE`**, which is never sent: pass `rpkBODY`/`rpkFIELD`.
- [ ] **A byte off the wire cast to an enum is checked first**, as an integer or as
      `Cardinal`: Delphi compares small enums as signed bytes, so
      `Ord(x) > Ord(High(T))` lets 128..255 through.
- [ ] **Every class registered in `initialization` is unregistered in `finalization`**
      (`UnregisterEngine`, `UnregisterDatabase`, `UnregisterStorageLink`,
      `UnregisterCompress`).
- [ ] **Algorithms exist only if their unit is linked** (compressors, storages,
      engines register themselves): look things up, never assume.
- [ ] **Header and cookie values** that come from a request go out through the params
      (which strip CR/LF/NUL), never concatenated into a header line.
- [ ] **User-facing strings** are constants in `src/languages/ralconsts_*.inc`: a new
      one goes into all three files (UTF-8 with BOM, edited byte-exact).

## Threads and performance

- [ ] Shared state is locked; a check-then-insert holds one lock across both steps
      (`TRALStringListSafe.Lock`), not two separate calls.
- [ ] A global cache of managed strings is filled at `initialization` or under a lock,
      never "whoever asks first".
- [ ] Per-request code on Delphi stays in `StringRAL`: `RALSameName`, `RALTrim`,
      `RALPosText`, not `SameText`/`Trim`/`LowerCase` (each converts UTF-8 to UTF-16
      and back - `W1057` in the build log).
- [ ] No string literal assigned per request on Delphi (it is copied): use a global
      filled once.
- [ ] Anything a native library calls on its own thread sets `IsMultiThread := True`.

## Portability and form

- [ ] Compiles on Delphi and FPC, on every platform the package claims
      (`ral-portability`, then `ral-check-unit`).
- [ ] Every declaration documented, members in the standard order (`ral-document`).
- [ ] A new unit is in its package files, `pasdoc.pds` and `PROJECT_MAP.md`
      (`ral-packages`).
- [ ] English everywhere: identifiers, `///`, comments, commit message.

## The proposal

Hand the change over with: **why** (bug, performance, feature); **files** touched;
**the exact change** per method; **risk** (Delphi/Lazarus, threads, wire format,
forms); **how it was verified** (which compiles, which test). Say plainly what was not
run.
