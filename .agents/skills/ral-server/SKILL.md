---
name: ral-server
description: >
  Builds a REST server with PascalRAL 2.0: choosing the engine, creating routes and
  handlers, path and query params, answering, linking the plugins a 2.0 server needs
  (compression, CORS, limits, security), serving files and Swagger. Use for "create an
  API", "add an endpoint/route", "REST server in Delphi/Lazarus", or a TRALServer
  descendant in an application.
---

# A REST server with RAL

## Pick the engine

The engine is the `TRALServer` descendant; the routes and handlers are the same on all.

| engine (unit) | Delphi | Lazarus | notes |
|---|---|---|---|
| `TRALIndyServer` (`RALIndyServer`) | yes | yes | needs Indy; OpenSSL DLLs for HTTPS |
| `TRALSynopseServer` (`RALSynopseServer`) | 2009+ | yes | mORMot2; `Mode`: `smThreads`, `smAsync` (many idle connections), `smHttpSys` (Windows kernel, HTTP/2) |
| `TRALfpHttpServer` (`RALfpHTTPServer`) | no | yes | nothing to install |
| `TRALSaguiServer` (`RALSaguiServer`) | 2009+ | yes | needs `libsagui` |
| `TRALMsQuicServer` (`RALMsQuicServer`) | yes | yes | QUIC, RAL clients only |
| `TRALCGIServer` (`RALCGIServer`) | yes | yes | behind IIS/Apache/nginx |
| `TRALFastCGIServer` (`RALFastCGIServer`) | yes | yes | behind nginx/Apache/IIS over FastCGI (TCP); needs `RALNetPackage` |
| `TRALWebBrokerServer` (`RALWebBrokerServer`) | yes | yes | a library: ISAPI (IIS) or Apache 2.4 module on Delphi, Apache 2.4 on Lazarus |
| `TRALInProcServer` (`RALInProc`) | yes | yes | no socket: tests with a `TRALClient` on `inproc://name` |
| `TRALUniGUIServer` | yes | no | inside a UniGUI application |

Lazarus on Linux: `cthreads` first in the program's `uses`.

## Routes and handlers

```pascal
uses
  RALServer, RALIndyServer, RALRequest, RALResponse, RALTypes, RALConsts, RALMIMETypes;

FServer := TRALIndyServer.Create(Self);
FServer.Port := 8000;
FServer.CreateRoute('ping', {$IFDEF FPC}@{$ENDIF}Ping, 'answers pong').Allow([amGET]);
FServer.CreateRoute('clients/:id', {$IFDEF FPC}@{$ENDIF}GetClient).Allow([amGET]);
FServer.Active := True;

procedure TForm1.Ping(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_OK, 'pong', rctTEXTPLAIN);
end;

procedure TForm1.GetClient(ARequest: TRALRequest; AResponse: TRALResponse);
begin
  AResponse.Answer(HTTP_OK, Format('{"id":%d}', [ARequest.ParamByName('id').AsInteger]));
end;
```

- The handler is `procedure(ARequest: TRALRequest; AResponse: TRALResponse)`, a method
  (`TRALOnReply`) or a plain procedure (`TRALOnReplyGen`). Do not invent variants.
- `Allow([...])` limits the methods (others answer 405 with `Allow`); `SkipAuth([...])`
  makes them public when authentication is on. Both chain. Two routes may share a
  path with different methods.
- Routes can also be made in the Object Inspector: `Routes`, Add, `Route`, `OnReply`.
- Prefer the constants: `HTTP_OK`, `HTTP_Created`, `HTTP_NoContent`, `HTTP_BadRequest`,
  `HTTP_Unauthorized`, `HTTP_NotFound` (`RALConsts`); `rctAPPLICATIONJSON`,
  `rctTEXTPLAIN`, `rctTEXTHTML` (`RALMIMETypes`). `Answer` with no type is JSON.
- Reading the request (query, path params, fields, headers, body, uploads) and
  answering streams and files: `ral-body`.

## Typed params, groups, versions

```pascal
FServer.CreateRoute('users/:id<int:range(1,9999)>', {$IFDEF FPC}@{$ENDIF}GetUser);
FServer.CreateRoute('files/*path', {$IFDEF FPC}@{$ENDIF}Files);          // the rest of the path
FServer.RouteGroups.AddGroup('api', '/api/v1').SkipAuthMethods := [amGET];
FServer.CreateRoute('orders', {$IFDEF FPC}@{$ENDIF}Orders).InGroup('api'); // /api/v1/orders
```

- A typed or constrained path param (`<int>`, `<number>`, `<bool>`, `<date>`,
  `<regex(...)>`, `range`, `length`, `values`, or the same in `URIParams`) is checked
  by RAL: a value it refuses makes the route not match, so the handler never sees it.
  `:name?` is optional at the end; `*name` takes the rest.
- Regular expressions are `RALRegex` (RE2 syntax, linear time, no backreferences or
  lookaround), the same on Delphi and FPC.
- A group's `OnBeforeExecute` (and a route's) runs before the handler; answering 400 or
  more there ends the request. Plugins are server-wide; `IncludePaths`/`ExcludePaths`
  limit one to some paths.
- `amQUERY` (a GET with a body), `ApiVersioning` (`ARequest.ApiVersion`, route and
  group `Versions`), `PathBase` (behind a proxy subpath).
- Anonymous handlers where the compiler has them: `uses RALRoutesAnon`, then
  `FServer.Get('/ping', procedure(ARequest: TRALRequest; AResponse: TRALResponse) begin ... end)`.

## Answer in pieces (streaming, Server-Sent Events)

```pascal
procedure TForm1.Progress(ARequest: TRALRequest; AResponse: TRALResponse);
var
  vInt: Integer;
begin
  for vInt := 1 to 10 do
  begin
    if not AResponse.SendEvent(Format('{"step":%d}', [vInt]), 'progress') then
      Exit; // the client left
    Sleep(500);
  end;
end;
```

- The first `Send`/`SendEvent` sends status and headers: set them before it. After it,
  `Answer`, `StatusCode`, `AddHeader` raise `ERALResponseCommitted`.
- `SendEvent` makes it `text/event-stream`; `Send` keeps the type you set (NDJSON:
  `rctAPPLICATIONNDJSON`). `Send` returns False once the client is gone.
- `AResponse.Detach` keeps the stream open after the handler returns, to be written
  from other threads (`IRALResponseStream`); pings every `StreamKeepAlive` ms.
- Mark the route `Streaming := True`: Sagui needs it to run the handler on a thread of
  its own, and Swagger/Postman show it as `text/event-stream`.
- Live on fpHTTP, Indy, Sagui, CGI (Lazarus) and Synopse `smThreads` (with the mORMot2
  change in `.agents/MORMOT2_PR_STREAMING.md`); other engines send it whole at the end.
- Compressed only with `TRALCompressPlugin.CompressStreams`; `TRALConcurrencyPlugin.MaxStreams`
  caps open streams (503); the AES body cipher refuses a stream.

## A 2.0 server has no features until you link plugins

Without plugins it answers routes and nothing else: **no compression, no cipher, no
CORS headers, no limits, no login**. Link what the application needs (component on the
form, `Server` property set; or `Plugin.Server := FServer` in code):

| need | plugin (unit) |
|---|---|
| gzip/deflate answers | `TRALCompressPlugin` (`RALContent`) + `RALCompressZLib` linked |
| browser calls from another site | `TRALCORSPlugin` (`RALCORS`): `Options.AllowOrigin` (empty = nobody) |
| a request size limit (413) | `TRALLimitsPlugin` (`RALContent`): `MaxRequestSize` |
| JSON body as params | `TRALJSONBodyPlugin` (`RALContent`) |
| security headers, IP lists, brute force, flood, `../` | `TRALSecurityHeadersPlugin`, `TRALWhiteListPlugin`, `TRALBlackListPlugin`, `TRALBruteForcePlugin`, `TRALFloodPlugin`, `TRALPathTraversalPlugin` (`RALSecurity`) |
| a cap on concurrent requests (503 + `Retry-After`) | `TRALConcurrencyPlugin` (`RALConcurrency`) |
| access log, trace context, Prometheus `/metrics`, `/health/live` and `/health/ready` | `TRALLogPlugin`, `TRALTracePlugin`, `TRALMetricsPlugin`, `TRALHealthPlugin` (`RALObservability`, package `RALPluginObservabilityPackage`) |
| rate limit (429), HTTPS redirect, maintenance (503), `X-HTTP-Method-Override` | `TRALRateLimitPlugin`, `TRALHttpsRedirectPlugin`, `TRALMaintenancePlugin`, `TRALMethodOverridePlugin` (`RALTraffic`, package `RALPluginTrafficPackage`) |
| response cache, `ETag` and 304 | `TRALResponseCachePlugin` (`RALResponseCache`, package `RALPluginCachePackage`) |
| the real client behind a proxy (NGINX, a cloud balancer) | `TRALForwardedPlugin` (`RALForwarded`, package `RALPluginForwardedPackage`): `TrustedProxies` |
| a reverse proxy / load balancer in front of other servers | `TRALUpstream` + `TRALProxyModule` (`RALProxyCore`, `RALProxyModule`, package `RALModuleProxyPackage`) |
| errors as `application/problem+json` | `Server.ErrorFormat := efProblemJSON`, `Server.ExceptionMap` (no plugin) |
| login | `ral-auth` |
| HTTPS | `ral-https` |

The order they run in is their `Priority`, not the order you link them.

## Objects and controllers

A route can take its body as an object: `CreateObjectRoute` with `BodyClass` and
`OnReplyObject`, `ValidationRules` on the route, `ResponseClass` for Swagger (see
`ral-body`). On Delphi 2010+, `TRALControllerModule.RegisterController(TClass)` turns
a class with `[Path]`, `[GET('/{id}')]`, `[POST]`... into routes (literal paths first,
one controller per request, `ExceptionMap` for errors); `TRALRestProxy.Make<I>` is the
matching client. `tools/ralopenapi` writes classes and a client from an OpenAPI JSON.

## Modules

- Static files and a site: `TRALWebModule` (package `RALModuleWebPackage`):
  `DocumentRoot` (empty serves nothing), `BlockedExtensions`, `CacheControl`.
- Swagger UI and Postman export: `TRALSwaggerModule` (`RALModuleSwaggerPackage`, which
  requires `RALPluginAuthPackage`). Route descriptions and `InputParams` feed it.
- Database over HTTP: `ral-dbware`. SOAP: `ral-xml-soap`. Your own: `ral-module`.

## Running as a service

`RALHostingPackage`: `TRALServiceHost` (`Server`, `ServiceName`; `Host.Run` reads
`install`/`uninstall`/`start`/`stop`/`console` and otherwise runs as the Windows service
or the systemd `Type=notify` daemon), `TRALSystemLog` (syslog/journal, Event Log;
`LogPlugin` takes a `TRALLogPlugin`'s lines), `TRALConfig` (`Bind(Server, 'Server')`:
JSON/INI/.env, `RAL_SERVER__PORT`, `--Server.Port=8080`), `TRALScheduler` (`Jobs` by
cron or interval, linked to the server).

## Server properties worth knowing

`Port` (8000; 0 = a free port, read it in `BoundPort`), `IPConfig` (bind addresses,
IPv6), `ShowServerStatus` (answers `/`), `Shutdown(30000)` (stops after the running
requests end; new ones get 503), `OnShutdown`,
`HideErrorDetails` (a 500 says only "Internal Server Error" - turn on in production),
`OnServerError` (log exceptions), `OnRequest`/`OnResponse`, `CookieLife` (minutes).

## Common errors

| symptom | cause |
|---|---|
| 404 | path spelled differently; `:id` needs a value |
| 405 | method not in `Allow` |
| browser blocks the call (CORS) | no `TRALCORSPlugin`, or `AllowOrigin` empty |
| answer not compressed | no `TRALCompressPlugin`, or `RALCompressZLib` not linked |
| `ERALResponseCommitted` | `Answer`/`StatusCode`/`AddHeader` after the first `Send` |
| events arrive all at once | engine without live streaming, or a proxy buffering (nginx: `X-Accel-Buffering: no` is sent) |
| form from 1.x does not open ("Property CompressType does not exist") | server properties moved to plugins: `ral-migrate` |

Guides (Portuguese): wiki
[rotas](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/rotas),
[plugins](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/plugins),
[qual-motor](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/qual-motor),
[resposta-em-fluxo](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/resposta-em-fluxo),
[servico](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/servico),
[fastcgi](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/fastcgi),
[iis-apache](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/iis-apache),
[configuracao](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/configuracao),
[testes-em-memoria](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/testes-em-memoria).
