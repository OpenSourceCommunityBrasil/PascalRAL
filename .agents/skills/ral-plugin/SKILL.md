---
name: ral-plugin
description: >
  Creates a PascalRAL server plugin (middleware): anything that is not a route -
  validation, headers, limits, logging, a new authentication scheme - as a TRALPlugin
  descendant with phases and priority. Use for "middleware", "check every request",
  "add a header to every response", "new auth scheme", or a TRALServerPlugin subclass.
---

# Write a plugin

A server with no plugin still answers its routes. Everything else is a plugin the
application links to the server (`Plugin.Server := Server` or `Server.AddPlugin`).
Base classes: `TRALServerPlugin` and `TRALPlugin` (publishes `Server`) in
`src/core/RALPlugin.pas`. Read that unit's header before writing one.

For a group of **routes**, write a module instead (`ral-module`).

## Phases: only the ones you declare are called

Override `Phases` (protected) and the hooks of those phases:

| phase | hook | runs | for |
|---|---|---|---|
| `ppValidate` | `ValidateRequest(ARequest, AResponse)` | before the body is decoded | refusing early: a status >= 400 ends the request; the body is never decoded |
| `ppProcess` | `ProcessRequest(ARequest, AResponse, var AHandled)` | before the modules | changing request or response; `AHandled := True` answers here |
| `ppResolveRoute` | `ResolveRoute(ARequest, AResponse): TRALRoute` | when the route is looked up | offering a route of the plugin's own (the JWT `/gettoken`), answered in `ProcessRequest` where `ARequest.RouteOwner = Self` |
| `ppAuthenticate` | `Authenticate(ARequest, AResponse, ARoute): TRALAuthResult` | when a route needs a login | being an authentication scheme (`arAccepted`, `arUnauthorized`, `arForbidden`) |
| `ppAuthResult` | `AfterAuthenticate`, `AuthAttempt` | after the login verdict | reacting to it (brute force counts failures here) |

`ppResolveRoute`, `ppAuthenticate` and `ppAuthResult` are calls between plugins, not
loops of the server. A phase no plugin declares costs nothing. If `Phases` depends on a
property, call `Changed` when it changes.

Lifecycle: `ServerActivating` (before the engine opens the port; an exception keeps the
server stopped) and `ServerDeactivating` (requests may still be running; its exceptions
go to `OnServerError`).

## Priority

Higher runs first; ties keep the order of linking. Override `class function
DefaultPriority`. The RAL's (`RALPriority*` in `RALPlugin`): security headers 950,
limits 900, compression 890, crypto 880, white list 870, black list 860, brute force
850, flood 840, path traversal 830, CORS 700, authentication 500, JSON body 400,
default 100, concurrency 50 (last of the validators).

## Example

```pascal
type
  /// Refuses requests without the expected API key header.
  TMyApiKeyPlugin = class(TRALPlugin)
  private
    FKey: StringRAL;
  protected
    function Phases: TRALPluginPhases; override;
  public
    procedure ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse); override;
  published
    /// Value the X-Api-Key header must carry.
    property Key: StringRAL read FKey write FKey;
  end;

function TMyApiKeyPlugin.Phases: TRALPluginPhases;
begin
  Result := [ppValidate];
end;

procedure TMyApiKeyPlugin.ValidateRequest(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  if not RALSameSecret(ARequest.GetHeader('X-Api-Key'), FKey) then
    AResponse.Answer(HTTP_Unauthorized, '{"error":"invalid key"}');
end;
```

Units: `RALPlugin`, `RALRequest`, `RALResponse`, `RALTypes`, `RALConsts`, `RALTools`.

## Rules

- **One object serves every thread**: state shared between requests (lists, counters)
  needs a lock, and a check followed by an insert holds one lock across both.
- **Secrets** are compared with `RALSameSecret`.
- **Configurable in the Object Inspector**: settings are published properties with a
  `default` equal to what the constructor sets (`ral-rad-component`).
- `Enabled := False` takes the plugin out of the path without unlinking it.
- An authentication plugin descends from `TRALAuthServer` (`src/core/RALAuthBase.pas`)
  and says, through `AttemptOf`, which refusals are guesses brute force should count.
  The existing schemes (`src/plugins/auth`) are the reference.
- Where it goes: a plugin of the core goes in `src/plugins` (package `PascalRAL`), an
  optional one in its own package (`src/plugins/<name>`, `RALPlugin<Name>Package`);
  see `ral-packages`. Palette `RAL - Plugins`.

User guide (Portuguese): wiki
[criar-plugin](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/criar-plugin)
and [plugins](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/plugins).
