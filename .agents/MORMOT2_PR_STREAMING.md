# mORMot2 pull request: chunked (streamed) responses in THttpServer

What RAL 2.0 changed in mORMot2 so that `TRALSynopseServer` can send a response
while the handler is still running (Server-Sent Events, NDJSON, progress). The
changes are generic - nothing RAL inside - and meant to be proposed upstream as
one pull request. Until it is merged, RAL compiles against a stock mORMot2 too:
`RALSynopseServer.pas` uses the hook only under
`{$IF DECLARED(THttpServerChunkedState)}`, and without it a stream is buffered
and sent when the handler returns.

Base: mORMot2 `32064e245` (2026-07-16). Files: `mormot.net.http.pas` and
`mormot.net.server.pas`, in mORMot2's `net` folder.

## The problem

`THttpServerRequest` only knows a complete body: `OutContent` (or a file /
progressive static stream) is set by the handler and sent after `Request()`
returns, with a `Content-Length`. A body of unknown length, produced over time
by the handler itself, has no way out - every Server-Sent Events server on top
of mORMot2 has to keep the whole answer in memory or bypass the class.

## The API

On `THttpServerRequest`:

| member | what it does |
| --- | --- |
| `BeginChunkedResponse: boolean` | sends status and headers now (`RespStatus`, `OutCustomHeaders`, `OutContentType`, as for a normal answer); any `OutContent` already set goes as the first chunk. False when the server class does not support it - the handler then fills `OutContent` as usual. |
| `SendChunk(Data, Len): boolean` | sends one piece. False when the connection failed: the body is aborted. |
| `EndChunkedResponse(Aborted = false)` | ends the body (`0\r\n\r\n`). Called implicitly after `Request()` returns. `Aborted = true` closes the connection without the last chunk - how HTTP tells a client the body was cut. |
| `ChunkedConnected: boolean` | whether the client is still there, without sending (a quiet stream would otherwise only find out at its next write). |
| `ChunkedState` | `hcrNone`, `hcrStarted`, `hcrEnded`, `hcrAborted`. |

In `mormot.net.http.pas`, the new response flag `rfChunkedResponse` makes
`THttpRequestContext.CompressContentAndFinalizeHead` write
`Transfer-Encoding: chunked` and no `Content-Length` (HTTP/1.0: no chunking,
`Connection: Close`, the end of the connection ends the body). No compression
is applied to such a response.

## Where it works

Only `THttpServer` (the threads mode, `useHttpSocket`) by now: `Process` stores
the client socket in the request (`fChunkedSocket`) before `DoRequest`, and the
methods write straight to it with `TrySndLow`. After `DoRequest`, a request that
went chunked ends its body, keeps or drops keep-alive (`hfConnectionClose`,
HTTP/1.0, aborted body) and runs `OnAfterResponse`, then leaves `Process`.

`THttpAsyncServer` and `THttpApiServer` (http.sys) return False from
`BeginChunkedResponse` - the request is answered whole, as today. Possible
follow-ups, not in this PR:
- async: a response whose `Write` is fed by the handler thread through the
  connection's output buffer (the `HTTP_ASYNCRESPONSE` machinery);
- http.sys: `HttpSendHttpResponse` with `HTTP_SEND_RESPONSE_FLAG_MORE_DATA`,
  then `HttpSendResponseEntityBody` per chunk.

## Points for review

- The early `exit` from `THttpServer.Process` after a chunked response skips the
  byte counters updated after the `finally` (`fStats`): a chunked response is
  not counted in the bytes-sent statistics. Counting it means adding the sizes
  in `SendChunk`; left out to keep the change small.
- `SendChunk` and `ChunkedConnected` may be called from another thread than the
  one running `Request()` (RAL lets a handler hand the stream over and return);
  the thread that runs `Process` waits inside `Request()` while that happens, so
  the socket is used by one thread at a time.
- HEAD gets the headers only. `hsoBan40xIP` is not checked for a chunked
  response (the status went out before the handler ended).

## How it was tested

On Linux (FPC 3.2, x86_64), RAL's live streaming test against
`TRALSynopseServer` in threads mode: 70/70 - first chunk leaves before the
handler ends, chunked framing and keep-alive after the last chunk, HTTP/1.0
delimited by close, SSE with pings, a stream written from another thread, a
client that leaves is detected (`ChunkedConnected`), 1 GB in pieces with
constant memory (RSS +300 KB), 50 open streams closed by stopping the server
(~100 ms). Against a stock mORMot2, and in async mode, the same handler answers
buffered with a `Content-Length`.

Note found on the way (not changed): `THttpServer` drops any request without a
`Host` header, HTTP/1.0 included, without an answer.

## The diff

```diff
diff --git a/src/net/mormot.net.http.pas b/src/net/mormot.net.http.pas
index bfc6b8401..9dcdc6646 100644
--- a/src/net/mormot.net.http.pas
+++ b/src/net/mormot.net.http.pas
@@ -339,6 +339,8 @@ type
   /// map the output state for THttpRequestContext.ResponseFlags
   // - separated from THttpRequestHeaderFlags so that they would both be stored
   // and accessed as a single byte - which is faster than word on Intel CPUs
+  // - rfChunkedResponse is set by THttpServerRequest.BeginChunkedResponse: the
+  // body has no known length and is sent in pieces after the headers
   THttpRequestResponseFlags = set of (
     rfAcceptRange,
     rfWantRange,
@@ -346,7 +348,8 @@ type
     rfHttp10,
     rfContentStreamNeedFree,
     rfAsynchronous,
-    rfProgressiveStatic);
+    rfProgressiveStatic,
+    rfChunkedResponse);
 
   /// define THttpRequestContext.ProcessBody response
   // - hrpSend should try to send the Dest buffer content
@@ -4102,7 +4105,17 @@ begin
   if (rfAcceptRange in ResponseFlags) and
       not (hhAcceptRangeBytes in HeadCustom) then
     result^.AppendShort('Accept-Ranges: bytes'#13#10);
-  if ContentStream = nil then // <> nil e.g. for rfProgressiveStatic
+  if rfChunkedResponse in ResponseFlags then
+  begin
+    // body of unknown length, sent in pieces by the caller after these headers:
+    // HTTP/1.1 chunks it, HTTP/1.0 delimits it by closing the connection
+    ContentLength := 0;
+    if rfHttp10 in ResponseFlags then
+      include(HeaderFlags, hfConnectionClose)
+    else
+      result^.AppendShort('Transfer-Encoding: chunked'#13#10);
+  end
+  else if ContentStream = nil then // <> nil e.g. for rfProgressiveStatic
   begin
     // same logic than THttpSocket.CompressDataAndWriteHeaders below
     if (integer(CompressAcceptHeader) <> 0) and
@@ -4140,7 +4153,8 @@ begin
     result^.Append(ContentEncoding^.Name);
     result^.AppendCRLF;
   end;
-  if not (hhContentLength in HeadCustom) then
+  if not (hhContentLength in HeadCustom) and
+     not (rfChunkedResponse in ResponseFlags) then
   begin
     result^.AppendShort('Content-Length: ');
     result^.Append(ContentLength);
diff --git a/src/net/mormot.net.server.pas b/src/net/mormot.net.server.pas
index 2aba80618..9b957e8b8 100644
--- a/src/net/mormot.net.server.pas
+++ b/src/net/mormot.net.server.pas
@@ -382,10 +382,24 @@ type
   /// a generic input/output structure used for HTTP server requests
   // - URL/Method/InHeaders/InContent properties are input parameters
   // - OutContent/OutContentType/OutCustomHeader are output parameters
+  /// the state of a THttpServerRequest chunked response
+  // - hcrNone is a regular response, built from OutContent once Request() returned
+  // - hcrStarted means BeginChunkedResponse sent the headers: SendChunk() may be
+  // called, and the body ends when Request() returns (or at EndChunkedResponse)
+  // - hcrEnded and hcrAborted mean the body was finished, normally or not
+  THttpServerChunkedState = (
+    hcrNone,
+    hcrStarted,
+    hcrEnded,
+    hcrAborted);
+
   THttpServerRequest = class(THttpServerRequestAbstract)
   protected
     fServer: THttpServerGeneric;
     fConnectionAsyncHandle: TConnectionAsyncHandle;
+    fChunkedState: THttpServerChunkedState;
+    fChunkedSocket: TObject; // a THttpServerSocket, set by THttpServer.Process
+
     fErrorMessage: string;
     fTempWriter: TJsonWriter; // reused between SetOutJson() calls
     {$ifdef USEWININET}
@@ -413,8 +427,32 @@ type
     /// prepare one reusable HTTP State Machine for sending the response
     function SetupResponse(var Context: THttpRequestContext;
       CompressGz, MaxSizeAtOnce: integer): PRawByteStringBuffer;
+    /// send the response status and headers now, for a body of unknown length
+    // which is then sent in pieces by SendChunk() while Request() is running
+    // - e.g. for Server-Sent Events (text/event-stream) or progress feedback
+    // - RespStatus, OutCustomHeaders and OutContentType are used as for a
+    // regular response; any OutContent already set is sent as first chunk
+    // - HTTP/1.1 uses "Transfer-Encoding: chunked", HTTP/1.0 closes the connection
+    // at the end of the body; there is no compression of such a response
+    // - returns false if this server class does not support it (only THttpServer
+    // does by now): the response should then be filled into OutContent as usual
+    function BeginChunkedResponse: boolean; virtual;
+    /// send one piece of a body started by BeginChunkedResponse
+    // - returns false if the connection failed: the body is then aborted
+    function SendChunk(Data: pointer; Len: PtrInt): boolean; virtual;
+    /// finish a body started by BeginChunkedResponse
+    // - implicitly called with Aborted=false once Request() returned
+    // - Aborted=true closes the connection without the last chunk, which is how
+    // HTTP tells the client the body was truncated
+    procedure EndChunkedResponse(Aborted: boolean = false); virtual;
+    /// check if the client of a body started by BeginChunkedResponse is still
+    // connected, without sending anything
+    // - a stream which stays quiet for a while (e.g. waiting for an event to be
+    // sent) would otherwise find out only when its next SendChunk() fails
+    function ChunkedConnected: boolean; virtual;
     /// just a wrapper around fErrorMessage := FormatString()
     procedure SetErrorMessage(const Fmt: RawUtf8; const Args: array of const);
+
     /// append a "Repr-Digest:" HTTP header to the request output
     procedure SetOutReprDigest(const Hash: THashDigest);
     /// serialize a given value as JSON into OutContent and OutContentType fields
@@ -447,6 +485,9 @@ type
     /// optional error message which will be used by SetupResponse
     property ErrorMessage: string
       read fErrorMessage write fErrorMessage;
+    /// the state of a response sent by BeginChunkedResponse/SendChunk
+    property ChunkedState: THttpServerChunkedState
+      read fChunkedState;
     {$ifdef USEWININET}
     /// for THttpApiServer, input parameter containing the caller full URL
     property FullUrl: SynUnicode
@@ -3304,6 +3345,8 @@ begin
   fOutContent := '';
   FastAssignNew(fOutContentType);
   FastAssignNew(fOutCustomHeaders);
+  fChunkedState := hcrNone;
+  fChunkedSocket := nil;
   fAuthenticationStatus := hraNone;
   fInternalFlags := [];
   FastAssignNew(fAuthenticatedUser);
@@ -3466,6 +3509,94 @@ begin
   // now TAsyncConnectionsSockets.Write(result) should be called
 end;
 
+function THttpServerRequest.BeginChunkedResponse: boolean;
+var
+  sock: THttpServerSocket;
+  output: PRawByteStringBuffer;
+  first: RawByteString;
+begin
+  result := false;
+  if (fChunkedState <> hcrNone) or
+     (fChunkedSocket = nil) then
+    exit; // only THttpServer.Process sets fChunkedSocket (by now)
+  sock := THttpServerSocket(fChunkedSocket);
+  first := fOutContent; // SetupResponse() would make it a Content-Length body
+  fOutContent := '';
+  include(sock.Http.ResponseFlags, rfChunkedResponse);
+  if fRespStatus = 0 then
+    fRespStatus := HTTP_SUCCESS;
+  output := SetupResponse(sock.Http, {compressgz=}-1,
+    THttpServer(fServer).fServerSendBufferSize);
+  if not sock.TrySndLow(output.Buffer, output.Len) then
+  begin
+    fChunkedState := hcrAborted;
+    sock.fKeepAliveClient := false;
+    exit;
+  end;
+  fChunkedState := hcrStarted;
+  result := true;
+  if first <> '' then
+    SendChunk(pointer(first), length(first));
+end;
+
+function THttpServerRequest.SendChunk(Data: pointer; Len: PtrInt): boolean;
+var
+  sock: THttpServerSocket;
+  tmp: TRawByteStringBuffer;
+begin
+  result := fChunkedState = hcrStarted;
+  if not result or
+     (Len <= 0) then
+    exit; // a zero-sized chunk would end the body
+  sock := THttpServerSocket(fChunkedSocket);
+  if PCardinal(sock.Http.CommandMethod)^ = HEAD_32 then
+    exit; // HEAD: only the headers
+  if rfHttp10 in sock.Http.ResponseFlags then
+    result := sock.TrySndLow(Data, Len) // delimited by the connection end
+  else
+  begin
+    // "<hexsize>"#13#10"<data>"#13#10 in a single syscall
+    tmp.Reset;
+    tmp.AppendShort(ShortString(IntToHex(Len, 1)));
+    tmp.AppendCRLF;
+    tmp.Append(Data, Len);
+    tmp.AppendCRLF;
+    result := sock.TrySndLow(tmp.Buffer, tmp.Len);
+  end;
+  if not result then
+    EndChunkedResponse({aborted=}true);
+end;
+
+procedure THttpServerRequest.EndChunkedResponse(Aborted: boolean);
+var
+  sock: THttpServerSocket;
+begin
+  if fChunkedState <> hcrStarted then
+    exit;
+  sock := THttpServerSocket(fChunkedSocket);
+  if Aborted then
+  begin
+    fChunkedState := hcrAborted;
+    sock.fKeepAliveClient := false; // close without the last chunk
+    exit;
+  end;
+  fChunkedState := hcrEnded;
+  if rfHttp10 in sock.Http.ResponseFlags then
+    sock.fKeepAliveClient := false // the end of the connection ends the body
+  else if (PCardinal(sock.Http.CommandMethod)^ <> HEAD_32) and
+          not sock.TrySndLow(PAnsiChar('0'#13#10#13#10), 5) then
+    sock.fKeepAliveClient := false;
+end;
+
+function THttpServerRequest.ChunkedConnected: boolean;
+begin
+  result := (fChunkedState = hcrStarted) and
+            not (THttpServerSocket(fChunkedSocket).SockReceivePending(0) in
+              [cspSocketError, cspSocketClosed, cspDataAvailableOnClosedSocket]);
+  if not result then
+    EndChunkedResponse({aborted=}true);
+end;
+
 procedure THttpServerRequest.SetErrorMessage(const Fmt: RawUtf8;
   const Args: array of const);
 begin
@@ -5069,7 +5200,18 @@ begin
     LockedInc32(@fCurrentProcess);
     // compute the response
     req.Prepare(ClientSock.Http, ClientSock.fRemoteIP, fAuthorize);
+    req.fChunkedSocket := ClientSock; // for BeginChunkedResponse/SendChunk
     DoRequest(req);
+    if req.fChunkedState <> hcrNone then
+    begin
+      // the response was sent in pieces during Request(): finish its body
+      req.EndChunkedResponse;
+      if hfConnectionClose in ClientSock.Http.HeaderFlags then
+        ClientSock.fKeepAliveClient := false;
+      if Assigned(fOnAfterResponse) then
+        DoAfterResponse(req, ClientSock, started);
+      exit; // the finally block below does the cleanup
+    end;
     output := req.SetupResponse(
       ClientSock.Http, fCompressGz, fServerSendBufferSize);
     if (hsoBan40xIP in fOptions) and
```
