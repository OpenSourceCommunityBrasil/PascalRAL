---
name: ral-mcp
description: >
  Builds an MCP (Model Context Protocol) server or client with PascalRAL 2.0:
  TRALMCPServer tools, resources and prompts, the stdio and Streamable HTTP
  transports, input requests (MRTR), tasks, OAuth metadata and the DBWare bridge.
  Use for "expose my Delphi/Lazarus app to an AI assistant", "MCP server",
  "MCP tool", or a client that calls MCP servers.
---

# MCP server and client

The MCP package (`RALModuleMCPPackage`, `src/modules/mcp`, requires
`RALModuleJSONRPCPackage`) speaks only the revision in `cRALMCPProtocolVersion`
(2026-07-28): stateless, no `initialize` and no session. A client of an older revision
gets `UnsupportedProtocolVersion` (-32022) with the list it may use.

## Server

| want | use |
|---|---|
| a tool | `Server.Tools.AddTool(name, description, OnCall)`; `Params` build the input schema (`Header` = `x-mcp-header`), `OutputSchema` + `AResult.SetStructured` |
| an answer | `AResult.AddText`, `AddImage`, `AddAudio`, `AddResourceLink`, `AddResourceText`/`Blob`; `Fail` = `isError` (what the model reads and fixes) |
| ask the user | `ACall.Elicit(key, message, schema)` and `Exit`; the client calls again and `ACall.InputResponse(key)` holds the answer; `SetState`/`State` carry data between rounds, signed with `StateKey` |
| progress or logs | `ACall.Progress`, `ACall.Log` (only reach the client on a live stream with its token/level) |
| cancellation | `ACall.Cancelled` in long loops |
| resources | `Resources` (fixed URI) and `ResourceTemplates` (URI template) with `OnRead`; a missing one answers -32602 |
| prompts | `Prompts` with `Arguments` and `OnGet` |
| long work | `Tool.RunAsTask` (tasks extension, only for clients that declare it) |
| who may call | `Tool.Roles` (identity of the request on HTTP) |
| database tools | `TRALMCPDBTools` (`RALModuleMCPDBPackage`): fixed SQL with declared params over a `TRALDBModule` |

Transports: `TRALMCPStdio.Run` in a console program (stdout holds only MCP; log to
stderr), or `TRALMCPModule` on a `TRALServer` (POST on `Path`, SSE when the request
asks for progress/logs or opens `subscriptions/listen`, on engines that stream live;
`AllowedOrigins`; `AuthorizationServers`/`ResourceURL` serve RFC 9728 metadata, and
`TRALServerOAuth2.ResourceMetadata := Module.ResourceMetadataURL`).

## Traps

- Batches are refused in MCP; the JSON-RPC module keeps them.
- `Notify*Changed` only reach clients with an open `subscriptions/listen`.
- An argument that does not fit the declared params is `isError`, not a protocol error.
- Engines that buffer the answer (Synopse `smAsync`/`smHttpSys`, MsQuic...) deliver
  progress and listens only at the end: use a live engine for them.

## Client

`TRALMCPClient` over a `TRALClient` (`Client`, `Path`) or a child process (`Command`,
`Arguments`; FPC and Delphi for Windows): `Discover`, `ListTools`, `CallTool`
(`OnInputRequest` answers elicitation), `ReadResource`, `GetPrompt`, `Request`.

Wiki (Portuguese): [mcp](https://github.com/OpenSourceCommunityBrasil/PascalRAL/wiki/mcp).
