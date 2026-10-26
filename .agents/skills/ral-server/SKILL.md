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
| login | `ral-auth` |
| HTTPS | `ral-https` |

The order they run in is their `Priority`, not the order you link them.

## Modules

- Static files and a site: `TRALWebModule` (package `RALModuleWebPackage`):
  `DocumentRoot` (empty serves nothing), `BlockedExtensions`, `CacheControl`.
- Swagger UI and Postman export: `TRALSwaggerModule` (`RALModuleSwaggerPackage`, which
  requires `RALPluginAuthPackage`). Route descriptions and `InputParams` feed it.
- Database over HTTP: `ral-dbware`. SOAP: `ral-xml-soap`. Your own: `ral-module`.

## Server properties worth knowing

`Port` (8000), `IPConfig` (bind addresses, IPv6), `ShowServerStatus` (answers `/`),
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
[resposta-em-fluxo](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/resposta-em-fluxo).
