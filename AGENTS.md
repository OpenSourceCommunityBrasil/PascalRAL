# AGENTS.md

Instructions for AI coding agents working with PascalRAL. Read this first; it points to
everything else.

## What PascalRAL is

PascalRAL (Pascal REST API Lite) is an Object Pascal **component suite** for building and
consuming REST APIs, installed into the Delphi (XE and later) and Lazarus/FPC IDEs from
one shared `src/` tree. It is a library: no `main`, no runnable binary and no test
runner in this repository. `PascalRAL` (runtime) + `PascalRALDsgn` (design) are the
mandatory pair, and at least one engine package (`<Engine>RAL`) is needed.

## Folders and packages (2.0)

- **One folder belongs to one package.** `src/core`, `common`, `compress`, `crypto` and
  `plugins` are `PascalRAL`; `src/design` is `PascalRALDsgn`; each optional package has
  its own folder (`src/plugins/auth`, `src/storage`, `src/modules/web`,
  `src/engine/indy`...). `src/others` holds only git submodules.
- Design-time units live in a `design/` subfolder of their feature.
- Packages: `PascalRAL`, `PascalRALDsgn`, engines `<Engine>RAL`, everything else
  `RAL<Category><Item>Package` / `RAL<Category><Item>Dsgn`, in `pkg/Delphi` and
  `pkg/Lazarus`. `.agents/PROJECT_MAP.md` maps every folder to its package and units.

## Rules

- **English everywhere**: identifiers, `///` doc comments, comments, commit messages.
- **Everything is documented** (pasdoc), and class members follow the standard order:
  skill `ral-document`.
- **Verifying a change means compiling the packages** on Delphi and FPC: `ral-build`,
  `ral-check-unit`. Read `src/`, never `compiled/` (build output, gitignored).
- **RAD first**: a feature is configurable in the Object Inspector (`ral-rad-component`).
- A change in `src/core` reaches every engine, module and downstream application:
  review it with `ral-review`.
- **Commit messages carry no AI or assistant attribution** (no `Co-Authored-By`, no
  session trailer), and the subject is a user-facing release note: `ral-commit`.
- Only Pascal tooling belongs to the project: no scripts in other languages. Checks
  that need a program are Pascal programs in `.agents/tools/`.
- When a recipe changes (build, packages, commit), update its skill in the same commit.

## Where to start

| goal | open first |
|---|---|
| server, routes, execution | `src/core/RALRoutes.pas`, `src/core/RALServer.pas` |
| client and engines | `src/core/RALClient.pas`, `src/engine/*` |
| request, response, params, body | `src/core/RALParams.pas`, `src/core/RALRequest.pas`, `src/core/RALResponse.pas` |
| plugins | `src/core/RALPlugin.pas`, `src/plugins/*` |
| authentication | `src/core/RALAuthBase.pas`, `src/plugins/auth/*` |
| multipart, JSON, streams | `src/common/RALMultipartCoder.pas`, `src/common/RALJson.pas`, `src/common/RALStream.pas` |
| compression, crypto | `src/compress/*`, `src/crypto/*` |
| storages | `src/storage/*` |
| DBWare | `src/database/RALDBModule.pas`, `src/database/*` |
| Web, Swagger, XML, SOAP | `src/modules/<module>/*` |

Start with one file, then open only what it leads to.

## Skills

Each skill is a folder in `.agents/skills/` with a `SKILL.md`. Agents that discover
skills load them by their description; others open the file when the task matches.

| skill | use it to |
|---|---|
| `ral-server` | build a REST server: engine, routes, handlers, the plugins a 2.0 server needs, modules |
| `ral-client` | call APIs with `TRALClient`: engines, sending, the two call styles, errors, threads |
| `ral-body` | read and write params, JSON, uploads, streams, compression, the AES body cipher |
| `ral-dbware` | expose a database over HTTP and use remote datasets |
| `ral-auth` | add Basic, JWT, Digest or OAuth2 login, public routes, login protection |
| `ral-https` | HTTPS on each engine, the self-signed certificate plugin, client pinning |
| `ral-xml-soap` | XML documents, datasets as XML, SOAP server and client |
| `ral-migrate` | move an application or package from RAL 1.x to 2.0 |
| `ral-build` | compile and install the Delphi and Lazarus packages; build traps |
| `ral-check-unit` | check a change compiles on every platform and on FPC without the IDE |
| `ral-packages` | add a unit, create a package, give a component its icon |
| `ral-commit` | write a commit message and know its CHANGELOG section |
| `ral-review` | review a change against the library's invariants |
| `ral-portability` | write code that works on Delphi and FPC, on every platform |
| `ral-debug` | find access violations, hangs, silent exits and leaks |
| `ral-document` | write doc comments, order class members, run pasdoc |
| `ral-plugin` | write a server plugin (middleware, auth scheme) |
| `ral-module` | write a module: a reusable group of routes |
| `ral-engine` | add or change a transport engine |
| `ral-rad-component` | design a feature for the Object Inspector |

## Tools for agents: `.agents/tools/`

Console programs in Pascal, RTL only, that turn fragile checks into exact ones. Not part
of the RAL packages. Build into `.agents/local/bin/`:

```bat
cd .agents\tools\<tool>
dcc32 -Q -B -NSSystem -E..\..\local\bin -N0..\..\local\bin\dcu <tool>.dpr
```

or with Lazarus, `lazbuild <tool>.lpi` (output in `.agents\local\bin\fpc`). Create
`.agents\local\bin\dcu` first for `dcc32`.

| tool | does |
|---|---|
| `bplunits` | lists the units inside each `.bpl` and flags units of another package or third-party units linked in (`ral-build`) |
| `commitsection` | prints the CHANGELOG section a commit message lands in (`ral-commit`) |
| `checkskills` | checks the skills and this file: frontmatter, names, the table above, cited paths, forbidden references; run it after changing a skill |

## Reference

- `CLAUDE.md`: architecture and traps in depth (Claude Code loads it; any agent may read
  the section it needs).
- `.agents/PROJECT_MAP.md`: every folder, package and unit.
- The wiki (Portuguese): https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki
- `src/engine/SSL.md`, `src/engine/HTTP2.md`, `src/engine/msquic/README.md`: engine guides.

## Local material: `.agents/local/`

`.agents/local/` is never committed (it is in `.gitignore`). New plans, notes, drafts,
caches and compiled tools go there, and nothing moves out of it without the
maintainer's say. Machine paths (the Delphi root, mORMot2, Lazarus, pasdoc) are in
`.agents/local/ENVIRONMENT.md`; its template is `.agents/ENVIRONMENT.example.md`. If
`.agents/local/LOCAL.md` exists, read it after this file.
