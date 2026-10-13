---
name: ral-client
description: >
  Calls REST APIs with PascalRAL's TRALClient: engine choice, BaseURL and failover,
  sending query/headers/body/files, the two calling styles and who frees the response,
  threads, timeouts, retries and error kinds. Use for "consume an API", "HTTP request
  from Delphi/Lazarus", "TRALClient", or a client that hangs the UI, leaks or loses a
  response.
---

# Calling an API with TRALClient

`TRALClient` talks to any HTTP server (RAL or not), with the same code on every engine.

## Engine

`EngineType` names the engine, and its unit must be in the `uses` (it registers itself):

| `EngineType` | unit | where |
|---|---|---|
| `'Indy'` | `RALIndyClient` | Delphi, Lazarus |
| `'mORMot2'` | `RALSynopseClient` | Delphi 2009+, Lazarus |
| `'netHTTP'` | `RALnetHTTPClient` | Delphi; no DLL on Windows |
| `'fpHTTP'` | `RALfpHTTPClient` | Lazarus |
| `'OkHttp'` | `RALOkHttpClient` | Delphi, Android (HTTP/2, pins) |
| `'MsQuic'`, `'Kwik'` | `RALMsQuicClient`, `RALKwikClient` | only against a RAL MsQuic server |

`Engine X not found` = the unit is missing from `uses`.

## Send

```pascal
Client.EngineType := 'netHTTP';
Client.BaseURL.Text := 'https://api.example.com';   // one URL per line = failover

Client.Request.Clear;
Client.Request.AddQuery('city', 'Recife');
Client.Request.AddHeader('X-Company', '12');
Client.Request.AddBody('{"name":"Ana"}', rctAPPLICATIONJSON);   // or AddField / AddFile
Client.Post('/clients', vResponse);
```

Methods: `Get`, `Post`, `Put`, `Patch`, `Delete`. Use `AddBody`/`AddField`/`AddFile`:
a param made with `AddValue` and no kind is `rpkNONE` and is never sent.

## Two ways to call - and who frees the response

**Wait for it** (`var AResponse` overloads): the response is **yours**, free it once.

```pascal
Client.Get('/clients/7', vResponse);
try
  if vResponse.StatusCode = HTTP_OK then
    Memo1.Text := vResponse.ResponseText;
finally
  vResponse.Free;
end;
```

A transport failure (server down, timeout) **raises**; the variable is set to nil
first, so the `finally` is safe.

**With an event** (`Get(route, OnResponse, behavior)`): the response belongs to RAL and
is freed after the event - use it there, never keep it.

```pascal
Client.Get('/clients/7', {$IFDEF FPC}@{$ENDIF}OnClient, ebMultiThread);

procedure TForm1.OnClient(ASender: TObject; AResponse: TRALResponse; AException: StringRAL);
begin
  if AException <> '' then
    ShowMessage(AException)
  else
    Memo1.Text := AResponse.ResponseText;
end;
```

| behavior | effect |
|---|---|
| `ebSingleThread` (default) | runs on the calling thread; the event fires before the call returns; **blocks the UI** |
| `ebMultiThread` | returns at once; the event fires later on the main thread. Use from buttons and on Android |

Anything that passes one of its methods as a callback and can be freed before the
answer calls `Client.DropCallbacks(Self)` in its destructor.

## Failures, retries, threads

- Detect a failure with `ErrorCode <> 0` or `TransportError` (`rteNone`, `rteConnect`,
  `rteTimeout`, `rteCertificate`, `rteCancelled`, `rteOther`), never with
  `StatusCode` (it is 0 when no HTTP answer arrived).
- With several `BaseURL` lines the client tries the next one on a connect failure (any
  method) or a timeout (idempotent methods only): a POST already sent is never resent.
- `OnBeforeExecute`/`OnAfterExecute` run per **attempt** (failover and the 401 retry
  are new attempts), on the request's thread.
- Each thread has its own `Request` (a copy of the creating thread's on first use):
  fill it, then call, on the same thread.
- `PoolConnection` (on) keeps engines and their connections between calls;
  `ShareConnection` (on) lets clients share one connection on netHTTP, OkHttp and QUIC.

## Useful properties

`ConnectTimeout` (30000 ms), `RequestTimeout` (10000 ms), `Authentication` (`ral-auth`),
`CompressType` (needs `RALCompressZLib` linked), `AcceptEncoding`, `SSL` and
`OnValidateServerCert` (`ral-https`), `HTTPVersion` (`rhv2` on netHTTP/OkHttp),
`MaxRedirects`, `UserAgent`, `SpoolAbove` (large answers to a temp file).

## Common errors

| symptom | cause |
|---|---|
| UI freezes during the call | `ebSingleThread`; pass `ebMultiThread` |
| access violation after the call | response freed twice, not freed, or kept from the event |
| server gets an empty body | `AddValue` without a kind |
| body arrives compressed/garbled | `RALCompressZLib` (or the zstd/brotli unit) not linked |

Guide (Portuguese): wiki
[cliente](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/cliente).
