---
name: ral-module
description: >
  Creates a PascalRAL module: a component that plugs a group of routes into a
  TRALServer, optionally with its own route class, its own request/response classes,
  its own error answer and server lifecycle hooks (as TRALDBModule, TRALWebModule,
  TRALSwaggerModule and TRALSOAPModule do). Use for "a reusable set of routes", a
  TRALModuleRoutes descendant, or "typed handlers for my routes".
---

# Write a module

Every module descends from `TRALModuleRoutes` (`src/core/RALServer.pas`), the RAL's and
a third party's alike, and links to a server through `Server`. `Domain` prefixes its
routes (`/db/opensql`). For a feature that is not a route, write a plugin
(`ral-plugin`).

## From the lightest to the deepest

| want | use |
|---|---|
| routes | `CreateRoute('path', Handler, 'description')` in the constructor |
| something around every route of the module | override `BeforeExecute` / `AfterExecute` |
| data or a handler of another type per route | `class function RouteClass` returning a `TRALRoute` descendant; `NewRoute(path, description)` creates one |
| request/response classes with methods of their own | `RequestClass` / `ResponseClass` (descendants of `TRALModuleRequest` / `TRALModuleResponse`) + override `ExecuteContext` to call the typed handler |
| an error answer of its own | `HandleException` returning True (it answered); False lets the server answer 500 and fire `OnServerError` |
| another default `Content-Type` | `DefaultContentType` (empty = JSON) |
| a route with no handler | `AnswerUnhandled` (404 by default) |
| react to the server starting/stopping | `ServerActivating` / `ServerDeactivating` |

## What to know

- **The module's request and response are wrappers**, not subclasses of
  `TRALRequest`/`TRALResponse`: they hold the core object (`Core`) and write to it, so
  the body is never copied. They are built only for routes **without** a core handler;
  a route made with `CreateRoute` (`OnReply`) gets the core objects, so one module can
  mix both kinds.
- An `Answer` added to the response class is declared `overload`, or it hides the
  inherited ones on both compilers.
- State of one request lives in the request class (`TRALDBRequest` takes a pooled
  connection on first use and gives it back in its destructor).
- OPTIONS, 405 and authentication are the core's: the handler never sees a method
  outside the route's `AllowedMethods`.
- `ServerActivating` runs before the engine opens the port; an exception keeps the
  server stopped. `ServerDeactivating` runs while requests may still be running.
  Neither runs at design time; a module loaded from a form hears the server in its
  `Loaded`.
- **Do not publish `Routes`** on a module that creates its routes in the constructor:
  the form would store them, and reloading recreates them without handlers.
- Routes are found by path first, then by method; a path some route has but not with
  that method answers 405 with `Allow`.
- Where it goes: `src/modules/<name>`, package `RALModule<Name>Package` (plus
  `RALModule<Name>Dsgn` when it has editors), palette `RAL - Modules`; see
  `ral-packages`. Settings are published properties (`ral-rad-component`).

## Reference implementations

- `src/database/RALDBModule.pas`: typed routes (`TRALDBRoute`, `CreateDBRoute`), request
  and response classes, `HandleException`, `ServerActivating` opening the pool.
- `src/modules/soap/RALSOAPModule.pas`: a whole protocol as a module, picking the
  operation in `CanAnswerRoute` before authentication, its own routes kept out of
  `Routes`, `DefaultContentType`.
- `src/modules/web/RALWebModule.pas`: `AnswerUnhandled` serving files.

Worked example with typed routes and response (Portuguese): wiki
[criar-modulo](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/criar-modulo).
