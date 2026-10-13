---
name: ral-body
description: >
  Reads and writes request and response content in PascalRAL: query, path params, form
  fields, headers, cookies, JSON bodies, multipart uploads and downloads, streams
  without copies, compression and the AES body cipher. Use for "read the JSON body",
  "upload a file", "return a PDF/stream", "multipart", "gzip", "encrypt the body", or a
  param that comes back nil or empty.
---

# Request and response content

Inside a route handler `ARequest` is what arrived and `AResponse` is what goes back.
Units: `RALRequest`, `RALResponse`, `RALParams`, `RALTypes`, `RALConsts`, `RALMIMETypes`.

## Reading the request

| want | how |
|---|---|
| query `?city=Recife` | `ARequest.GetQuery('city')` |
| path param `clients/:id` | `ARequest.ParamByName('id').AsInteger` |
| form field (urlencoded or multipart) | `ARequest.GetField('name')` |
| header | `ARequest.GetHeader('X-Company')` |
| the whole body as text (JSON, XML) | `ARequest.RequestText` |
| any of them by name | `ARequest.ParamByName('x')` - the first of that name, any kind |
| method, caller | `ARequest.Method`, `ARequest.ClientInfo.IP` |

- `Get...` answer `''` when absent; **`ParamByName` answers `nil`** - test before use.
- `TRALParam`: `AsString`, `AsInteger`, `AsInt64`, `AsDouble` (accepts `2.5` and
  `2,5`), `AsBoolean`, `AsDateTime`, `FileName`, `Content` (a stream - not always a
  `TMemoryStream`), `SaveToFile(folder, name)` (keeps only the last path component).
- A form becomes fields, not a body: read `GetField`, not `RequestText`.
- A JSON object body becomes fields only with `TRALJSONBodyPlugin` on the server.
- A value derived from the token must **replace** what the client sent:
  `Params.ReplaceParam`, not `AddParam` (`ParamByName` returns the first).

## Upload

```pascal
vFile := ARequest.ParamByName('file');
if (vFile = nil) or (vFile.FileName = '') then
  AResponse.Answer(HTTP_BadRequest, '{"error":"send the file field"}')
else
begin
  vFile.SaveToFile('C:\uploads', vFile.FileName);
  AResponse.Answer(HTTP_OK, '{"ok":true}');
end;
```

## Answering

| want | how |
|---|---|
| JSON | `AResponse.Answer(HTTP_OK, '{"id":7}')` (JSON is the default type) |
| text, HTML | `Answer(HTTP_OK, 'pong', rctTEXTPLAIN)`, `rctTEXTHTML` |
| status only | `Answer(HTTP_NoContent)` |
| a file from disk | `Answer('C:\reports\month.pdf')` (inline; `False` as 2nd param downloads) |
| your stream, copied | `Answer(HTTP_OK, vStream, 'application/pdf')` - you free yours |
| your stream, no copy | `Answer(HTTP_OK, vStream, 'application/pdf', True)` - RAL owns and frees it |
| write the body directly | `AResponse.BodyStream` |
| a header, a cookie | `AResponse.AddHeader(...)`, `AResponse.AddCookie(TRALCookie)` |

An unhandled exception becomes a 500 with its message and fires `OnServerError`;
`HideErrorDetails` on the server hides the message. Prefer answering the error
yourself with the right status.

## On the client side

`Client.Request.AddQuery`, `AddHeader`, `AddCookie`, `AddBody(text, type)`,
`AddField(name, value)`, `AddFile(path)` (multipart); read `AResponse.ResponseText` or
the params. Never `AddValue` without a kind (`rpkNONE` is not sent).

## Large bodies

There is one decoded body per request and per response; params read it without copies.
Bodies above 8 MB live in chunks, and with `SpoolAbove` (on `TRALLimitsPlugin` and
`TRALClient`) in a temporary file. Code doing `TMemoryStream(Param.Content).Memory`
breaks there: read the stream.

## Compression and cipher

- Compression needs its unit linked: `RALCompressZLib` (gzip, deflate, zlib), the zstd
  and brotli packages for those. A coding whose unit is not linked is never sent.
- A server compresses answers only with `TRALCompressPlugin`: its `CompressType` wins;
  `ctNone` follows the client's `Accept-Encoding`. Already-compressed types (images,
  zip, pdf...) are left alone.
- The AES body cipher works RAL-to-RAL: `TRALCriptoPlugin.Key` on the server,
  `Client.CriptoOptions` on the client, same key. It is AES-CBC with an HMAC; set
  `RALCriptoKeyDerivation := rkdPBKDF2` on **both** sides to derive the key with
  PBKDF2. HTTPS is still the transport protection (`ral-https`).

Guides (Portuguese): wiki
[pedido-resposta](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/pedido-resposta),
[corpo-unico](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/corpo-unico).
