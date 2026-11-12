{ The HTTP/2 server of RAL's own sockets: h2 over TLS (ALPN) and h2c with prior
  knowledge, framed by nghttp2 (loaded at run time, src/bindings/nghttp2.pas).
  One poll thread owns each connection and its nghttp2 session; the requests
  run on a pool of workers and hand their answers back to the connection
  through a locked queue, so the session is only ever touched by its thread. }
unit RALNgHttp2Server;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALServer, RALRequest, RALResponse, RALParams,
  RALCustomObjects, RALResponseStream, RALStream, RALSocket, RALTLS, RALSocketLoop,
  nghttp2;

type
  TRALNgHttp2Server = class;
  TRALNgHttp2Connection = class;
  TRALNgHttp2Stream = class;

  { TLS of the HTTP/2 server: the certificate as PEM files or text; the ALPN
    offered is h2 alone (D1: the server speaks only HTTP/2). }
  TRALNgHttp2SSL = class(TRALSSL)
  private
    FCertFile: StringRAL;
    FCertPEM: StringRAL;
    FKeyFile: StringRAL;
    FKeyPassword: StringRAL;
    FKeyPEM: StringRAL;
    FMinVersion: TRALTLSVersion;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    /// TLS 1.2 or newer by default.
    constructor Create;

    /// Certificate as PEM text (set by SetTLSCertificate, RALSelfSigned).
    property CertPEM: StringRAL read FCertPEM write FCertPEM;
    /// Key as PEM text.
    property KeyPEM: StringRAL read FKeyPEM write FKeyPEM;
  published
    /// Certificate (PEM, with its chain).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Private key of CertFile (PEM).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
    /// Password of KeyFile.
    property KeyPassword: StringRAL read FKeyPassword write FKeyPassword;
    /// Lowest TLS version accepted.
    property MinVersion: TRALTLSVersion read FMinVersion write FMinVersion default mtv12;
  end;

  { What a connection and the requests running for it share, counted by
    reference: the lock, the streams with something new to send, and the
    connection while it lives (a worker that ends after the connection closed
    finds nil and drops its answer). }
  TRALNgHttp2Link = class
  private
    /// The connection, nil once it closed.
    FConnection: TRALNgHttp2Connection;
    /// Guards everything here and the response side of the streams.
    FLock: TCriticalSection;
    /// Streams with a new head, data or end to hand to nghttp2.
    FReady: TList;
    /// References: the connection and each stream a worker holds.
    FRefCount: IntegerRAL;
  public
    /// The link of AConnection.
    constructor Create(AConnection: TRALNgHttp2Connection);
    destructor Destroy; override;

    /// Takes a reference.
    procedure AddRef;
    /// Queues AStream for its connection and wakes it (any thread).
    procedure Notify(AStream: TRALNgHttp2Stream);
    /// Drops a reference; the last frees the link.
    procedure Release;
  end;

  { One HTTP/2 stream: the request as it arrives (poll thread), then the answer
    as the worker writes it (under the link's lock) and nghttp2 reads it (poll
    thread). Counted by reference: the connection while the stream is open and
    the worker while the request runs. }
  TRALNgHttp2Stream = class
  private
    /// The authority (:authority or host).
    FAuthority: StringRAL;
    /// The request body.
    FBody: TMemoryStream;
    /// The bytes of the body, for MaxRequestSize.
    FBodySize: Int64RAL;
    /// The stream was closed by nghttp2 (sent, reset, or the connection went).
    FClosed: boolean;
    /// The data provider deferred: resume it when something comes.
    FDeferred: boolean;
    { Set when the queued output drops below the high water mark or the stream
      closes; created by the first writer that has to wait, under the lock. }
    FDrained: TEvent;
    /// The answer ended (no more data will come).
    FEnded: boolean;
    /// The response fields (name=value, lower case names).
    FHeaders: TStringList;
    /// The status and the headers are ready to submit.
    FHeadReady: boolean;
    /// The head was submitted to nghttp2.
    FHeadSent: boolean;
    /// The stream id.
    FId: Integer;
    /// The link of the connection.
    FLink: TRALNgHttp2Link;
    /// :method.
    FMethod: StringRAL;
    /// The answer has no body (HEAD, 204, 304).
    FNoBody: boolean;
    /// The bytes still to send.
    FOut: TMemoryStream;
    /// Where nghttp2 reads FOut next.
    FOutPos: Int64RAL;
    /// :path.
    FPath: StringRAL;
    /// Already in the link's ready list.
    FQueued: boolean;
    /// References.
    FRefCount: IntegerRAL;
    /// The request fields (name=value).
    FRequestHeaders: TStringList;
    /// :scheme.
    FScheme: StringRAL;
    /// A whole answer's body read in place (buffered answers), owned.
    FSource: TStream;
    /// A worker took the request.
    FStarted: boolean;
    /// The response status.
    FStatus: IntegerRAL;
    /// The trailer fields (name=value), sent after the body.
    FTrailers: TStringList;

    /// Bytes in FOut not yet read by nghttp2.
    function Pending: Int64RAL;
  public
    /// The stream AId of ALink.
    constructor Create(ALink: TRALNgHttp2Link; AId: Integer);
    destructor Destroy; override;

    /// Takes a reference.
    procedure AddRef;
    /// Drops a reference; the last frees the stream.
    procedure Release;

    /// The stream id.
    property Id: Integer read FId;
  end;

  { One client connection on a poll thread: TLS (when the server has it), the
    nghttp2 server session, the streams open on it. }
  TRALNgHttp2Connection = class(TRALSocketLoopConnection)
  private
    /// The bytes nghttp2 produced that the channel did not take yet.
    FBacklog: TMemoryStream;
    /// Where FBacklog is written next.
    FBacklogPos: IntegerRAL;
    /// The client address.
    FClientIP: StringRAL;
    /// The client port.
    FClientPort: IntegerRAL;
    /// The TLS handshake is done (or there is no TLS).
    FHandshaken: boolean;
    /// When the connection must have its preface or answer by (ms, RALSocketNow).
    FIdleSince: Int64RAL;
    /// The shared state with the workers.
    FLink: TRALNgHttp2Link;
    /// The server.
    FServer: TRALNgHttp2Server;
    /// The nghttp2 session.
    FSession: pnghttp2_session;
    /// The streams open, by id.
    FStreams: TList;
    /// The TLS channel, nil for h2c.
    FTLS: TRALTLSChannel;

    /// Hands the streams the workers made ready to nghttp2.
    procedure DrainReady;
    /// Asks nghttp2 for its output and writes it; False when the link broke.
    function PumpOutput: boolean;
    /// A stream's request is complete: hand it to a worker.
    procedure StartRequest(AStream: TRALNgHttp2Stream);
    /// Submits the head and the body provider of AStream.
    procedure SubmitHead(AStream: TRALNgHttp2Stream);
  protected
    procedure Closed; override;
  public
    /// The connection of AServer over ASocket; ASecure marks TLS.
    constructor Create(AServer: TRALNgHttp2Server; ASocket: TRALSocket; ASecure: boolean);
    destructor Destroy; override;

    function FlushOutput: boolean; override;
    function HasOutput: boolean; override;
    procedure Serve(ANow: Int64RAL); override;
    procedure Stopping; override;
  end;

  { The HTTP/2 server: h2 (SSL.Enabled, ALPN h2) or h2c with prior knowledge on
    Port. Requests run on PoolCount worker threads; MaxConcurrentStreams per
    connection; nghttp2's own limits against the known floods are on.
    SupportsLiveStream: a streamed answer goes out as DATA frames as it is
    written, and the response's Trailers go as HTTP/2 trailers (gRPC). }
  TRALNgHttp2Server = class(TRALServer)
  private
    FExecuteInPollThread: boolean;
    FIdleTimeout: IntegerRAL;
    FInitialWindowSize: IntegerRAL;
    FLibraryName: StringRAL;
    /// The listening socket.
    FListener: TRALSocket;
    /// The poll loop of the connections.
    FLoop: TRALSocketLoop;
    FMaxConcurrentStreams: IntegerRAL;
    FPollThreads: IntegerRAL;
    FPoolCount: IntegerRAL;
    /// The requests waiting for a worker.
    FQueue: TList;
    /// Signals a request in FQueue.
    FQueueEvent: TEvent;
    /// Guards FQueue.
    FQueueLock: TCriticalSection;
    /// The TLS context, nil for h2c.
    FTLSContext: TRALTLSContext;
    /// The worker threads.
    FWorkers: TList;

    /// A connection was accepted (accepting thread).
    procedure Accepted(ASocket: TRALSocket; ATag: IntegerRAL);
    function GetSSL: TRALNgHttp2SSL;
    /// Takes the next request for a worker; nil when the server stops.
    function NextRequest: TRALNgHttp2Stream;
    /// Queues a request for the workers.
    procedure QueueRequest(AStream: TRALNgHttp2Stream);
    /// Runs one request on a worker thread.
    procedure RunRequest(AStream: TRALNgHttp2Stream);
    procedure SetSSL(const AValue: TRALNgHttp2SSL);
  protected
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; override;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    class function SupportsLiveStream: boolean; override;
    function TLSProvisioning: TRALTLSProvisioning; override;
  published
    { Runs each request on the poll thread that read it, without the hand-off
      to a worker: faster for short handlers, but a slow one stalls the other
      connections of that thread (and a live stream waits on it). }
    property ExecuteInPollThread: boolean read FExecuteInPollThread
      write FExecuteInPollThread default False;
    /// Milliseconds a connection may stay with no stream open before GOAWAY; 0 = never.
    property IdleTimeout: IntegerRAL read FIdleTimeout write FIdleTimeout default 120000;
    /// The window given to each stream and to the connection, in bytes.
    property InitialWindowSize: IntegerRAL read FInitialWindowSize write FInitialWindowSize
      default 1048576;
    /// The nghttp2 library to load; empty takes the platform's usual names.
    property LibraryName: StringRAL read FLibraryName write FLibraryName;
    /// Streams a client may open at once on a connection.
    property MaxConcurrentStreams: IntegerRAL read FMaxConcurrentStreams
      write FMaxConcurrentStreams default 100;
    /// Poll threads (connections are spread over them); 0 = one per processor.
    property PollThreads: IntegerRAL read FPollThreads write FPollThreads default 0;
    /// Worker threads that run the requests; 0 = four per processor.
    property PoolCount: IntegerRAL read FPoolCount write FPoolCount default 0;
    /// TLS: h2 when enabled, h2c otherwise.
    property SSL: TRALNgHttp2SSL read GetSSL write SetSSL;
  end;

implementation

uses
  RALContent;

const
  /// Output queued per stream above which a live writer waits.
  cHighWater = 1024 * 1024;
  /// Headers HTTP/2 forbids in a message (RFC 9113 8.2.2).
  cHopByHop: array[0..5] of StringRAL = ('connection', 'keep-alive', 'proxy-connection',
    'transfer-encoding', 'upgrade', 'te');

type
  /// A worker thread of the server.
  TRALNgHttp2Worker = class(TThread)
  private
    /// The server that owns the worker.
    FServer: TRALNgHttp2Server;
  protected
    procedure Execute; override;
  public
    /// The worker of AServer.
    constructor Create(AServer: TRALNgHttp2Server);
  end;

  /// The live writer of a stream: queues bytes for its connection.
  TRALNgHttp2StreamWriter = class(TRALStreamWriter)
  private
    /// The response being streamed.
    FResponse: TRALResponse;
    /// The server of the stream.
    FServer: TRALNgHttp2Server;
    /// The stream written to.
    FStream: TRALNgHttp2Stream;
  public
    /// The writer of AStream that fills AResponse.
    constructor Create(AServer: TRALNgHttp2Server; AStream: TRALNgHttp2Stream;
      AResponse: TRALResponse);

    function Alive: boolean; override;
    { Fills the stream's status and fields from AResponse, under the link's
      lock, without handing them to the connection yet. }
    procedure FillHead(AResponse: TRALResponse; AStatusCode: IntegerRAL);
    procedure WriteEnd(AAborted: boolean); override;
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

  /// A list of name/value pairs in the layout nghttp2 reads.
  TRALNgHttp2Fields = record
    /// The fields.
    Items: array of nghttp2_nv;
    /// The bytes they point at.
    Texts: array of RawByteString;
  end;

/// Whether ANAme may not go in an HTTP/2 message.
function IsHopByHop(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  for vInt := Low(cHopByHop) to High(cHopByHop) do
    if RALSameName(cHopByHop[vInt], AName) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

/// Adds a field to AFields (the name in lower case).
procedure AddField(var AFields: TRALNgHttp2Fields; const AName, AValue: RawByteString);
var
  vCount: IntegerRAL;
begin
  vCount := Length(AFields.Items);
  SetLength(AFields.Items, vCount + 1);
  SetLength(AFields.Texts, (vCount + 1) * 2);
  AFields.Texts[vCount * 2] := RawByteString(LowerCase(string(AName)));
  AFields.Texts[vCount * 2 + 1] := AValue;
end;

/// Points the fields at their texts (after every AddField: the array may move).
procedure BindFields(var AFields: TRALNgHttp2Fields);
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to High(AFields.Items) do
  begin
    AFields.Items[vInt].name := PByte(Pointer(AFields.Texts[vInt * 2]));
    AFields.Items[vInt].namelen := Length(AFields.Texts[vInt * 2]);
    AFields.Items[vInt].value := PByte(Pointer(AFields.Texts[vInt * 2 + 1]));
    AFields.Items[vInt].valuelen := Length(AFields.Texts[vInt * 2 + 1]);
    AFields.Items[vInt].flags := NGHTTP2_NV_FLAG_NONE;
  end;
end;

{ nghttp2 callbacks: user_data is the connection; they run on its poll thread }

/// Starts a request when its HEADERS begin.
function OnBeginHeaders(session: pnghttp2_session; frame: Pointer;
  user_data: Pointer): Integer; cdecl;
var
  vConn: TRALNgHttp2Connection;
  vHd: pnghttp2_frame_hd;
  vStream: TRALNgHttp2Stream;
begin
  Result := 0;
  vConn := TRALNgHttp2Connection(user_data);
  vHd := pnghttp2_frame_hd(frame);
  if (vHd^.type_ <> NGHTTP2_HEADERS) or
     (pnghttp2_headers_frame(frame)^.cat <> NGHTTP2_HCAT_REQUEST) then
    Exit;
  vStream := TRALNgHttp2Stream.Create(vConn.FLink, vHd^.stream_id);
  vConn.FStreams.Add(vStream);
  nghttp2_session_set_stream_user_data(session, vHd^.stream_id, vStream);
end;

/// Receives a header of a request.
function OnHeader(session: pnghttp2_session; frame: Pointer; name: PByte;
  namelen: nghttp2_size_t; value: PByte; valuelen: nghttp2_size_t; flags: Byte;
  user_data: Pointer): Integer; cdecl;
var
  vStream: TRALNgHttp2Stream;
  vName, vValue: StringRAL;
begin
  Result := 0;
  vStream := TRALNgHttp2Stream(nghttp2_session_get_stream_user_data(session,
    pnghttp2_frame_hd(frame)^.stream_id));
  if (vStream = nil) or vStream.FStarted then
    Exit;
  SetString(vName, PAnsiChar(name), namelen);
  SetString(vValue, PAnsiChar(value), valuelen);
  if vName = ':method' then
    vStream.FMethod := vValue
  else if vName = ':path' then
    vStream.FPath := vValue
  else if vName = ':scheme' then
    vStream.FScheme := vValue
  else if vName = ':authority' then
    vStream.FAuthority := vValue
  else if (vName <> '') and (vName[1] <> ':') then
  begin
    if (vName = 'host') and (vStream.FAuthority = '') then
      vStream.FAuthority := vValue;
    vStream.FRequestHeaders.Add(string(vName) + '=' + string(vValue));
  end;
end;

/// Receives a chunk of a request body.
function OnDataChunk(session: pnghttp2_session; flags: Byte; stream_id: Integer;
  data: PByte; len: nghttp2_size_t; user_data: Pointer): Integer; cdecl;
var
  vStream: TRALNgHttp2Stream;
  vConn: TRALNgHttp2Connection;
  vLimits: TObject;
begin
  Result := 0;
  vConn := TRALNgHttp2Connection(user_data);
  vStream := TRALNgHttp2Stream(nghttp2_session_get_stream_user_data(session, stream_id));
  if (vStream = nil) or vStream.FStarted then
    Exit;
  Inc(vStream.FBodySize, len);
  // TRALLimitsPlugin.MaxRequestSize, before the bytes are kept
  vLimits := vConn.FServer.FindPlugin(TRALLimitsPlugin);
  if (vLimits <> nil) and (TRALLimitsPlugin(vLimits).MaxRequestSize > 0) and
     (vStream.FBodySize > TRALLimitsPlugin(vLimits).MaxRequestSize) then
  begin
    nghttp2_submit_rst_stream(session, 0, stream_id, NGHTTP2_REFUSED_STREAM);
    Exit;
  end;
  vStream.FBody.WriteBuffer(data^, len);
end;

/// Handles a received frame.
function OnFrameRecv(session: pnghttp2_session; frame: Pointer;
  user_data: Pointer): Integer; cdecl;
var
  vHd: pnghttp2_frame_hd;
  vStream: TRALNgHttp2Stream;
begin
  Result := 0;
  vHd := pnghttp2_frame_hd(frame);
  if (vHd^.type_ in [NGHTTP2_HEADERS, NGHTTP2_DATA]) and
     ((vHd^.flags and NGHTTP2_FLAG_END_STREAM) <> 0) then
  begin
    vStream := TRALNgHttp2Stream(nghttp2_session_get_stream_user_data(session, vHd^.stream_id));
    if (vStream <> nil) and not vStream.FStarted then
      TRALNgHttp2Connection(user_data).StartRequest(vStream);
  end;
end;

/// Drops the state of a closed stream.
function OnStreamClose(session: pnghttp2_session; stream_id: Integer;
  error_code: Cardinal; user_data: Pointer): Integer; cdecl;
var
  vConn: TRALNgHttp2Connection;
  vStream: TRALNgHttp2Stream;
begin
  Result := 0;
  vConn := TRALNgHttp2Connection(user_data);
  vStream := TRALNgHttp2Stream(nghttp2_session_get_stream_user_data(session, stream_id));
  if vStream = nil then
    Exit;
  nghttp2_session_set_stream_user_data(session, stream_id, nil);
  vConn.FLink.FLock.Acquire;
  try
    vStream.FClosed := True;
    vConn.FLink.FReady.Remove(vStream);
    vStream.FQueued := False;
    if vStream.FDrained <> nil then
      vStream.FDrained.SetEvent;
  finally
    vConn.FLink.FLock.Release;
  end;
  vConn.FStreams.Remove(vStream);
  if vConn.FStreams.Count = 0 then
    vConn.FIdleSince := RALSocketNow;
  vStream.Release;
end;

/// Supplies the response body to nghttp2.
function OnDataRead(session: pnghttp2_session; stream_id: Integer; buf: PByte;
  length: nghttp2_size_t; data_flags: PCardinal; source: pnghttp2_data_source;
  user_data: Pointer): nghttp2_ssize_t; cdecl;
var
  vStream: TRALNgHttp2Stream;
  vLink: TRALNgHttp2Link;
  vRead: Int64RAL;
  vFields: TRALNgHttp2Fields;
  vInt: IntegerRAL;
  vEnd: boolean;
begin
  vStream := TRALNgHttp2Stream(source^.ptr);
  vLink := vStream.FLink;
  vEnd := False;
  vLink.FLock.Acquire;
  try
    if vStream.FSource <> nil then
    begin
      // a whole answer, read where it is
      Result := vStream.FSource.Read(buf^, length);
      if Result <= 0 then
      begin
        Result := 0;
        vEnd := True;
      end;
    end
    else
    begin
      vRead := vStream.Pending;
      if vRead > Int64RAL(length) then
        vRead := length;
      if vRead > 0 then
      begin
        vStream.FOut.Position := vStream.FOutPos;
        vStream.FOut.ReadBuffer(buf^, vRead);
        Inc(vStream.FOutPos, vRead);
        // the queue empties: reuse its memory
        if vStream.FOutPos = vStream.FOut.Size then
        begin
          vStream.FOut.Clear;
          vStream.FOutPos := 0;
        end;
        if (vStream.Pending < cHighWater) and (vStream.FDrained <> nil) then
          vStream.FDrained.SetEvent;
        Result := vRead;
      end
      else if vStream.FEnded then
      begin
        Result := 0;
        vEnd := True;
      end
      else
      begin
        vStream.FDeferred := True;
        Result := NGHTTP2_ERR_DEFERRED;
        Exit;
      end;
      if (Result > 0) and vStream.FEnded and (vStream.Pending = 0) then
        vEnd := True;
    end;
    if vEnd then
    begin
      data_flags^ := data_flags^ or NGHTTP2_DATA_FLAG_EOF;
      if (vStream.FTrailers <> nil) and (vStream.FTrailers.Count > 0) then
      begin
        // trailers end the stream instead of the last DATA
        data_flags^ := data_flags^ or NGHTTP2_DATA_FLAG_NO_END_STREAM;
        vFields.Items := nil;
        vFields.Texts := nil;
        for vInt := 0 to vStream.FTrailers.Count - 1 do
          AddField(vFields, RawByteString(vStream.FTrailers.Names[vInt]),
            RawByteString(vStream.FTrailers.ValueFromIndex[vInt]));
        BindFields(vFields);
        nghttp2_submit_trailer(session, stream_id, @vFields.Items[0], System.Length(vFields.Items));
      end;
    end;
  finally
    vLink.FLock.Release;
  end;
end;

{ TRALNgHttp2SSL }

constructor TRALNgHttp2SSL.Create;
begin
  inherited Create;
  FMinVersion := mtv12;
end;

procedure TRALNgHttp2SSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALNgHttp2SSL then
  begin
    TRALNgHttp2SSL(Dest).FCertFile := FCertFile;
    TRALNgHttp2SSL(Dest).FCertPEM := FCertPEM;
    TRALNgHttp2SSL(Dest).FKeyFile := FKeyFile;
    TRALNgHttp2SSL(Dest).FKeyPassword := FKeyPassword;
    TRALNgHttp2SSL(Dest).FKeyPEM := FKeyPEM;
    TRALNgHttp2SSL(Dest).FMinVersion := FMinVersion;
  end;
end;

{ TRALNgHttp2Link }

constructor TRALNgHttp2Link.Create(AConnection: TRALNgHttp2Connection);
begin
  inherited Create;
  FConnection := AConnection;
  FLock := TCriticalSection.Create;
  FReady := TList.Create;
  FRefCount := 1;
end;

destructor TRALNgHttp2Link.Destroy;
begin
  FReady.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALNgHttp2Link.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

procedure TRALNgHttp2Link.Notify(AStream: TRALNgHttp2Stream);
var
  vConn: TRALNgHttp2Connection;
begin
  FLock.Acquire;
  try
    vConn := FConnection;
    if (vConn = nil) or AStream.FClosed then
      Exit;
    if not AStream.FQueued then
    begin
      AStream.FQueued := True;
      FReady.Add(AStream);
    end;
    { woken under the lock: the connection cannot go away meanwhile; its own
      thread drains the list after the read and needs no wake }
    if TThread.CurrentThread <> vConn.Thread then
      vConn.Wake;
  finally
    FLock.Release;
  end;
end;

procedure TRALNgHttp2Link.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

{ TRALNgHttp2Stream }

constructor TRALNgHttp2Stream.Create(ALink: TRALNgHttp2Link; AId: Integer);
begin
  inherited Create;
  FLink := ALink;
  FLink.AddRef;
  FId := AId;
  FRefCount := 1;
  FBody := TMemoryStream.Create;
  FOut := TMemoryStream.Create;
  FRequestHeaders := TStringList.Create;
  FHeaders := TStringList.Create;
  FStatus := HTTP_OK;
end;

destructor TRALNgHttp2Stream.Destroy;
begin
  FBody.Free;
  FOut.Free;
  FSource.Free;
  FRequestHeaders.Free;
  FHeaders.Free;
  FTrailers.Free;
  FDrained.Free;
  FLink.Release;
  inherited Destroy;
end;

procedure TRALNgHttp2Stream.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

function TRALNgHttp2Stream.Pending: Int64RAL;
begin
  Result := FOut.Size - FOutPos;
end;

procedure TRALNgHttp2Stream.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

{ TRALNgHttp2StreamWriter }

constructor TRALNgHttp2StreamWriter.Create(AServer: TRALNgHttp2Server;
  AStream: TRALNgHttp2Stream; AResponse: TRALResponse);
begin
  inherited Create;
  FServer := AServer;
  FStream := AStream;
  FResponse := AResponse;
  FramesItself := True;
  Chunked := False;
end;

function TRALNgHttp2StreamWriter.Alive: boolean;
begin
  FStream.FLink.FLock.Acquire;
  try
    Result := (not FStream.FClosed) and (FStream.FLink.FConnection <> nil);
  finally
    FStream.FLink.FLock.Release;
  end;
end;

procedure TRALNgHttp2StreamWriter.WriteEnd(AAborted: boolean);
var
  vLink: TRALNgHttp2Link;
begin
  vLink := FStream.FLink;
  vLink.FLock.Acquire;
  try
    if (FResponse.Trailers <> nil) and not AAborted then
    begin
      FStream.FTrailers := TStringList.Create;
      FStream.FTrailers.Assign(FResponse.Trailers);
    end;
    FStream.FEnded := True;
  finally
    vLink.FLock.Release;
  end;
  if AAborted then
    FStream.FStatus := -1;
  vLink.Notify(FStream);
end;

procedure TRALNgHttp2StreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo;
  AStatusCode: IntegerRAL);
begin
  FillHead(TRALResponse(AResponse), AStatusCode);
  FStream.FLink.Notify(FStream);
end;

procedure TRALNgHttp2StreamWriter.FillHead(AResponse: TRALResponse; AStatusCode: IntegerRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vCookies: TStringList;
  vResponse: TRALResponse;
  vHasCookies: boolean;
begin
  vResponse := AResponse;
  FStream.FLink.FLock.Acquire;
  try
    FStream.FStatus := AStatusCode;
    FStream.FHeaders.Clear;
    if vResponse.ContentType <> '' then
      FStream.FHeaders.Add('content-type=' + string(vResponse.ContentType));
    if vResponse.ContentEncoding <> '' then
      FStream.FHeaders.Add('content-encoding=' + string(vResponse.ContentEncoding));
    if vResponse.ContentDisposition <> '' then
      FStream.FHeaders.Add('content-disposition=' + string(vResponse.ContentDisposition));
    if vResponse.ContentEncription <> '' then
      FStream.FHeaders.Add('content-encription=' + string(vResponse.ContentEncription));
    vHasCookies := False;
    for vInt := 0 to vResponse.Params.Count - 1 do
    begin
      vParam := vResponse.Params.Index[vInt];
      if vParam.Kind = rpkCOOKIE then
        vHasCookies := True;
      if (vParam.Kind = rpkHEADER) and (vParam.ParamName <> '') and
         not IsHopByHop(vParam.ParamName) and
         not RALSameName(vParam.ParamName, 'content-length') then
        FStream.FHeaders.Add(LowerCase(string(vParam.ParamName)) + '=' +
          string(RALSafeHeaderText(vParam.AsString)));
    end;
    if vHasCookies then
    begin
      vCookies := TStringList.Create;
      try
        vResponse.GetParamsCookies(vCookies, IncMinute(Now, FServer.CookieLife));
        for vInt := 0 to vCookies.Count - 1 do
          FStream.FHeaders.Add('set-cookie=' + vCookies[vInt]);
      finally
        vCookies.Free;
      end;
    end;
    FStream.FNoBody := HeadOnly or (AStatusCode = HTTP_NoContent) or (AStatusCode = 304);
    FStream.FHeadReady := True;
  finally
    FStream.FLink.FLock.Release;
  end;
end;

function TRALNgHttp2StreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
var
  vLink: TRALNgHttp2Link;
  vWait: Cardinal;
begin
  vLink := FStream.FLink;
  Result := False;
  if ASize <= 0 then
  begin
    Result := Alive;
    Exit;
  end;
  // back pressure: wait while the queue is over the high water mark
  if SendTimeout > 0 then
    vWait := SendTimeout
  else
    vWait := INFINITE;
  while True do
  begin
    vLink.FLock.Acquire;
    try
      if FStream.FClosed or (vLink.FConnection = nil) then
        Exit;
      if FStream.Pending < cHighWater then
      begin
        FStream.FOut.Position := FStream.FOut.Size;
        FStream.FOut.WriteBuffer(ABuffer, ASize);
        Break;
      end;
      // the event exists only for a writer that had to wait
      if FStream.FDrained = nil then
        FStream.FDrained := TEvent.Create(nil, True, False, '')
      else
        FStream.FDrained.ResetEvent;
    finally
      vLink.FLock.Release;
    end;
    if FStream.FDrained.WaitFor(vWait) <> wrSignaled then
      Exit;
  end;
  vLink.Notify(FStream);
  Result := True;
end;

{ TRALNgHttp2Connection }

constructor TRALNgHttp2Connection.Create(AServer: TRALNgHttp2Server;
  ASocket: TRALSocket; ASecure: boolean);
var
  vCallbacks: pnghttp2_session_callbacks;
  vOption: pnghttp2_option;
  vSettings: array[0..2] of nghttp2_settings_entry;
  vRes: Integer;
begin
  inherited Create;
  FServer := AServer;
  FSocket := ASocket;
  FChannel := TRALSocketChannel.Create(ASocket, False);
  FClientIP := ASocket.PeerAddress;
  FClientPort := ASocket.PeerPort;
  FStreams := TList.Create;
  FBacklog := TMemoryStream.Create;
  FLink := TRALNgHttp2Link.Create(Self);
  FIdleSince := RALSocketNow;
  if ASecure then
  begin
    FTLS := TRALTLSChannel.Create(FChannel, AServer.FTLSContext);
    FChannel := FTLS;
  end
  else
    FHandshaken := True;
  nghttp2_session_callbacks_new(vCallbacks);
  try
    nghttp2_session_callbacks_set_on_begin_headers_callback(vCallbacks, OnBeginHeaders);
    nghttp2_session_callbacks_set_on_header_callback(vCallbacks, OnHeader);
    nghttp2_session_callbacks_set_on_data_chunk_recv_callback(vCallbacks, OnDataChunk);
    nghttp2_session_callbacks_set_on_frame_recv_callback(vCallbacks, OnFrameRecv);
    nghttp2_session_callbacks_set_on_stream_close_callback(vCallbacks, OnStreamClose);
    nghttp2_option_new(vOption);
    try
      nghttp2_option_set_peer_max_concurrent_streams(vOption, AServer.MaxConcurrentStreams);
      // the floods known: rapid reset and CONTINUATION
      if Assigned(nghttp2_option_set_stream_reset_rate_limit) then
        nghttp2_option_set_stream_reset_rate_limit(vOption, 1000, 33);
      if Assigned(nghttp2_option_set_max_continuations) then
        nghttp2_option_set_max_continuations(vOption, 8);
      vRes := nghttp2_session_server_new2(FSession, vCallbacks, Self, vOption);
    finally
      nghttp2_option_del(vOption);
    end;
  finally
    nghttp2_session_callbacks_del(vCallbacks);
  end;
  if vRes <> 0 then
    raise ERALSocketError.Create('nghttp2: ' + string(AnsiString(nghttp2_strerror(vRes))));
  vSettings[0].settings_id := NGHTTP2_SETTINGS_MAX_CONCURRENT_STREAMS;
  vSettings[0].value := AServer.MaxConcurrentStreams;
  vSettings[1].settings_id := NGHTTP2_SETTINGS_INITIAL_WINDOW_SIZE;
  vSettings[1].value := AServer.InitialWindowSize;
  vSettings[2].settings_id := NGHTTP2_SETTINGS_ENABLE_PUSH;
  vSettings[2].value := 0;
  nghttp2_submit_settings(FSession, NGHTTP2_FLAG_NONE, @vSettings[0], 3);
  if AServer.InitialWindowSize > NGHTTP2_INITIAL_WINDOW_SIZE then
    nghttp2_session_set_local_window_size(FSession, NGHTTP2_FLAG_NONE, 0,
      AServer.InitialWindowSize);
end;

destructor TRALNgHttp2Connection.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FStreams.Count - 1 do
    TRALNgHttp2Stream(FStreams[vInt]).Release;
  FStreams.Free;
  if FSession <> nil then
    nghttp2_session_del(FSession);
  FBacklog.Free;
  FChannel.Free;
  FSocket.Free;
  FLink.Release;
  inherited Destroy;
end;

procedure TRALNgHttp2Connection.Closed;
var
  vInt: IntegerRAL;
  vStream: TRALNgHttp2Stream;
begin
  // the workers stop seeing this connection; their writers wake and give up
  FLink.FLock.Acquire;
  try
    FLink.FConnection := nil;
    FLink.FReady.Clear;
    for vInt := 0 to FStreams.Count - 1 do
    begin
      vStream := TRALNgHttp2Stream(FStreams[vInt]);
      vStream.FClosed := True;
      vStream.FQueued := False;
      if vStream.FDrained <> nil then
        vStream.FDrained.SetEvent;
    end;
  finally
    FLink.FLock.Release;
  end;
  inherited Closed;
end;

procedure TRALNgHttp2Connection.DrainReady;
var
  vList: TList;
  vInt: IntegerRAL;
  vStream: TRALNgHttp2Stream;
  vSubmit, vResume: boolean;
begin
  vList := TList.Create;
  try
    FLink.FLock.Acquire;
    try
      vList.Assign(FLink.FReady);
      FLink.FReady.Clear;
      for vInt := 0 to vList.Count - 1 do
        TRALNgHttp2Stream(vList[vInt]).FQueued := False;
    finally
      FLink.FLock.Release;
    end;
    for vInt := 0 to vList.Count - 1 do
    begin
      vStream := TRALNgHttp2Stream(vList[vInt]);
      if vStream.FClosed then
        Continue;
      FLink.FLock.Acquire;
      try
        vSubmit := vStream.FHeadReady and not vStream.FHeadSent;
        vResume := vStream.FHeadSent and vStream.FDeferred and
          ((vStream.Pending > 0) or vStream.FEnded);
        if vResume then
          vStream.FDeferred := False;
      finally
        FLink.FLock.Release;
      end;
      if vStream.FStatus < 0 then
      begin
        // a stream that failed after its head: reset, the way HTTP/2 says "cut"
        nghttp2_submit_rst_stream(FSession, 0, vStream.FId, NGHTTP2_INTERNAL_ERROR);
        Continue;
      end;
      if vSubmit then
        SubmitHead(vStream)
      else if vResume then
        nghttp2_session_resume_data(FSession, vStream.FId);
    end;
  finally
    vList.Free;
  end;
end;

function TRALNgHttp2Connection.FlushOutput: boolean;
begin
  Result := PumpOutput;
end;

function TRALNgHttp2Connection.HasOutput: boolean;
begin
  Result := (FBacklog.Size > FBacklogPos) or
    ((FSession <> nil) and FHandshaken and (nghttp2_session_want_write(FSession) <> 0));
end;

function TRALNgHttp2Connection.PumpOutput: boolean;
var
  vData: PByte;
  vLen: nghttp2_ssize_t;
  vWritten: IntegerRAL;
begin
  Result := True;
  if not FHandshaken then
    Exit;
  // what the channel did not take last time goes first
  while FBacklog.Size > FBacklogPos do
  begin
    vWritten := FChannel.Write(PByte(NativeUInt(FBacklog.Memory) + NativeUInt(FBacklogPos))^,
      FBacklog.Size - FBacklogPos);
    if vWritten = RALSocketWouldBlock then
      Exit;
    if vWritten <= 0 then
      Exit;
    Inc(FBacklogPos, vWritten);
  end;
  FBacklog.Clear;
  FBacklogPos := 0;
  while True do
  begin
    vLen := nghttp2_session_mem_send(FSession, vData);
    if vLen < 0 then
    begin
      Result := False;
      Exit;
    end;
    if vLen = 0 then
      Break;
    vWritten := FChannel.Write(vData^, vLen);
    if vWritten = RALSocketWouldBlock then
      vWritten := 0;
    if vWritten < vLen then
    begin
      // keep the rest: mem_send's buffer lives only until the next call
      FBacklog.WriteBuffer(PByte(NativeUInt(vData) + NativeUInt(vWritten))^, vLen - vWritten);
      Break;
    end;
  end;
  FChannel.Flush;
end;

procedure TRALNgHttp2Connection.Serve(ANow: Int64RAL);
var
  vBuffer: array[0..32767] of Byte;
  vRead: IntegerRAL;
  vRes: nghttp2_ssize_t;
begin
  if (Returned and RALPollErr) <> 0 then
  begin
    Close;
    Exit;
  end;
  if not FHandshaken then
  begin
    try
      FHandshaken := FTLS.HandshakeStep;
    except
      Close;
      Exit;
    end;
    if not FHandshaken then
    begin
      if ANow - FIdleSince > 10000 then
        Close;
      Exit;
    end;
    // ALPN must have settled on h2
    if FTLS.SelectedALPN <> NGHTTP2_PROTO_VERSION_ID then
    begin
      Close;
      Exit;
    end;
  end;
  // read what is there
  repeat
    vRead := FChannel.Read(vBuffer, SizeOf(vBuffer));
    if vRead = 0 then
    begin
      Close;
      Exit;
    end;
    if vRead > 0 then
    begin
      vRes := nghttp2_session_mem_recv(FSession, @vBuffer[0], vRead);
      if vRes < 0 then
      begin
        // a protocol error: nghttp2 queued GOAWAY; send it and close
        PumpOutput;
        Close;
        Exit;
      end;
      FIdleSince := ANow;
    end;
  until (vRead <> SizeOf(vBuffer)) and not FChannel.HasBuffered;
  DrainReady;
  if not PumpOutput then
  begin
    Close;
    Exit;
  end;
  if (nghttp2_session_want_read(FSession) = 0) and (nghttp2_session_want_write(FSession) = 0) and
     (FBacklog.Size = FBacklogPos) then
  begin
    Close;
    Exit;
  end;
  if (FServer.IdleTimeout > 0) and (FStreams.Count = 0) and
     (ANow - FIdleSince > FServer.IdleTimeout) then
  begin
    nghttp2_session_terminate_session(FSession, NGHTTP2_NO_ERROR);
    PumpOutput;
    Close;
  end;
end;

procedure TRALNgHttp2Connection.StartRequest(AStream: TRALNgHttp2Stream);
begin
  AStream.FStarted := True;
  if FServer.FExecuteInPollThread then
  begin
    // on this thread: the answer is drained right after the read
    FServer.RunRequest(AStream);
    Exit;
  end;
  AStream.AddRef;
  FServer.QueueRequest(AStream);
end;

procedure TRALNgHttp2Connection.Stopping;
begin
  if (FSession <> nil) and FHandshaken then
  begin
    nghttp2_session_terminate_session(FSession, NGHTTP2_NO_ERROR);
    PumpOutput;
  end;
end;

procedure TRALNgHttp2Connection.SubmitHead(AStream: TRALNgHttp2Stream);
var
  vFields: TRALNgHttp2Fields;
  vProvider: nghttp2_data_provider;
  vInt: IntegerRAL;
  vNoBody: boolean;
begin
  vFields.Items := nil;
  vFields.Texts := nil;
  FLink.FLock.Acquire;
  try
    AddField(vFields, ':status', RawByteString(IntToStr(AStream.FStatus)));
    for vInt := 0 to AStream.FHeaders.Count - 1 do
      AddField(vFields, RawByteString(AStream.FHeaders.Names[vInt]),
        RawByteString(AStream.FHeaders.ValueFromIndex[vInt]));
    AStream.FHeadSent := True;
    vNoBody := AStream.FNoBody and ((AStream.FTrailers = nil) or (AStream.FTrailers.Count = 0));
  finally
    FLink.FLock.Release;
  end;
  BindFields(vFields);
  if vNoBody then
    nghttp2_submit_response(FSession, AStream.FId, @vFields.Items[0], Length(vFields.Items), nil)
  else
  begin
    vProvider.source.ptr := AStream;
    vProvider.read_callback := OnDataRead;
    nghttp2_submit_response(FSession, AStream.FId, @vFields.Items[0], Length(vFields.Items),
      @vProvider);
  end;
end;

{ TRALNgHttp2Worker }

constructor TRALNgHttp2Worker.Create(AServer: TRALNgHttp2Server);
begin
  FServer := AServer;
  inherited Create(False);
end;

procedure TRALNgHttp2Worker.Execute;
var
  vStream: TRALNgHttp2Stream;
begin
  while not Terminated do
  begin
    vStream := FServer.NextRequest;
    if vStream = nil then
      Break;
    try
      FServer.RunRequest(vStream);
    finally
      vStream.Release;
    end;
  end;
end;

{ TRALNgHttp2Server }

constructor TRALNgHttp2Server.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueue := TList.Create;
  FQueueLock := TCriticalSection.Create;
  FQueueEvent := TEvent.Create(nil, False, False, '');
  FWorkers := TList.Create;
  FMaxConcurrentStreams := 100;
  FInitialWindowSize := 1048576;
  FIdleTimeout := 120000;
  SetEngine('NgHttp2');
end;

destructor TRALNgHttp2Server.Destroy;
begin
  Active := False;
  FWorkers.Free;
  FQueueEvent.Free;
  FQueueLock.Free;
  FQueue.Free;
  inherited Destroy;
end;

procedure TRALNgHttp2Server.Accepted(ASocket: TRALSocket; ATag: IntegerRAL);
var
  vConn: TRALNgHttp2Connection;
begin
  try
    vConn := TRALNgHttp2Connection.Create(Self, ASocket, ATag = 1);
  except
    ASocket.Free;
    Exit;
  end;
  FLoop.Adopt(vConn);
end;

function TRALNgHttp2Server.CreateRALSSL: TRALSSL;
begin
  Result := TRALNgHttp2SSL.Create;
end;

function TRALNgHttp2Server.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  if AResponse.EngineContext is TRALNgHttp2Stream then
  begin
    Result := TRALNgHttp2StreamWriter.Create(Self, TRALNgHttp2Stream(AResponse.EngineContext),
      AResponse);
    Result.HeadOnly := ARequest.Method = amHEAD;
  end
  else
    Result := nil;
end;

function TRALNgHttp2Server.GetSSL: TRALNgHttp2SSL;
begin
  Result := TRALNgHttp2SSL(GetDefaultSSL);
end;

function TRALNgHttp2Server.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  ACertificate.PrivateKeyPassword := SSL.KeyPassword;
  ACertificate.CertificatePEM := SSL.CertPEM;
  ACertificate.PrivateKeyPEM := SSL.KeyPEM;
  Result := (SSL.CertFile <> '') or (SSL.CertPEM <> '');
end;

function TRALNgHttp2Server.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

function TRALNgHttp2Server.NextRequest: TRALNgHttp2Stream;
begin
  Result := nil;
  while Result = nil do
  begin
    FQueueLock.Acquire;
    try
      if FQueue.Count > 0 then
      begin
        Result := TRALNgHttp2Stream(FQueue[0]);
        FQueue.Delete(0);
        if FQueue.Count > 0 then
          FQueueEvent.SetEvent;
        Exit;
      end;
      if not Active then
        Exit;
    finally
      FQueueLock.Release;
    end;
    FQueueEvent.WaitFor(500);
    if TThread.CurrentThread is TRALNgHttp2Worker then
      if TRALNgHttp2Worker(TThread.CurrentThread).Terminated then
        Exit;
  end;
end;

procedure TRALNgHttp2Server.QueueRequest(AStream: TRALNgHttp2Stream);
begin
  FQueueLock.Acquire;
  try
    FQueue.Add(AStream);
  finally
    FQueueLock.Release;
  end;
  FQueueEvent.SetEvent;
end;

procedure TRALNgHttp2Server.RunRequest(AStream: TRALNgHttp2Stream);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vInt: IntegerRAL;
  vName, vValue, vCookies: StringRAL;
  vWire: TStream;
  vWriter: TRALNgHttp2StreamWriter;
  vConn: TRALNgHttp2Connection;
  vType: StringRAL;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  try
    try
      vRequest.Query := AStream.FPath;
      vRequest.Method := HTTPMethodToRALMethod(AStream.FMethod);
      vRequest.ProtocolVersion := rhv2;
      vRequest.Host := AStream.FAuthority;
      if AStream.FScheme = 'https' then
        vRequest.HttpVersion := 'HTTPS'
      else
        vRequest.HttpVersion := 'HTTP';
      AStream.FLink.FLock.Acquire;
      try
        vConn := AStream.FLink.FConnection;
        if vConn <> nil then
        begin
          vRequest.ClientInfo.IP := vConn.FClientIP;
          vRequest.ClientInfo.Port := vConn.FClientPort;
          vRequest.ClientInfo.ConnectionID := Int64(NativeUInt(vConn));
        end;
      finally
        AStream.FLink.FLock.Release;
      end;
      vCookies := '';
      for vInt := 0 to AStream.FRequestHeaders.Count - 1 do
      begin
        vName := StringRAL(AStream.FRequestHeaders.Names[vInt]);
        vValue := StringRAL(AStream.FRequestHeaders.ValueFromIndex[vInt]);
        if vName = 'content-type' then
          vRequest.ContentType := vValue
        else if vName = 'content-encoding' then
          vRequest.ContentEncoding := vValue
        else if vName = 'accept-encoding' then
          vRequest.AcceptEncoding := vValue
        else if vName = 'content-encription' then
          vRequest.ContentEncription := vValue
        else if vName = 'accept-encription' then
          vRequest.AcceptEncription := vValue
        else if vName = 'user-agent' then
          vRequest.ClientInfo.UserAgent := vValue
        else if vName = 'cookie' then
        begin
          // HTTP/2 may split the cookie in several fields
          if vCookies <> '' then
            vCookies := vCookies + '; ';
          vCookies := vCookies + vValue;
        end;
        vRequest.Params.AddParam(vName, vValue, rpkHEADER);
      end;
      if vCookies <> '' then
        vRequest.AddCookies(vCookies);
      vRequest.ContentSize := AStream.FBody.Size;
      DecodeAuth(vRequest);
      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        AStream.FBody.Position := 0;
        vRequest.SetWireBody(AStream.FBody, boBorrowed);
      end;
      vResponse.EngineContext := AStream;
      ProcessCommands(vRequest, vResponse);
      if not FinishStream(vRequest, vResponse) then
      begin
        // an empty body keeps the handler's type (a gRPC trailers-only answer)
        vType := vResponse.ContentType;
        vWire := vResponse.TakeWireStream;
        if vResponse.ContentType = '' then
          vResponse.ContentType := vType;
        vRequest.Finish;
        // the head through the same code as a live answer, then the body in place
        vWriter := TRALNgHttp2StreamWriter.Create(Self, AStream, vResponse);
        try
          vWriter.HeadOnly := vRequest.Method = amHEAD;
          vWriter.FillHead(vResponse, vResponse.StatusCode);
          AStream.FLink.FLock.Acquire;
          try
            if vWire <> nil then
              AStream.FHeaders.Add('content-length=' + IntToStr(vWire.Size));
            if (vWire <> nil) and (vWire.Size > 0) and not vWriter.HeadOnly then
            begin
              vWire.Position := 0;
              AStream.FSource := vWire;
              vWire := nil;
            end
            else
              AStream.FNoBody := True;
            if (vResponse.Trailers <> nil) and (vResponse.Trailers.Count > 0) then
            begin
              AStream.FTrailers := TStringList.Create;
              AStream.FTrailers.Assign(vResponse.Trailers);
            end;
            AStream.FEnded := True;
          finally
            AStream.FLink.FLock.Release;
          end;
          vWire.Free;
          AStream.FLink.Notify(AStream);
        finally
          vWriter.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        // nothing out yet: a 500; else the stream is reset
        AStream.FLink.FLock.Acquire;
        try
          if not AStream.FHeadReady then
          begin
            AStream.FStatus := HTTP_InternalError;
            AStream.FHeaders.Clear;
            AStream.FHeaders.Add('content-type=text/plain');
            AStream.FHeadReady := True;
            AStream.FEnded := True;
            AStream.FNoBody := True;
          end
          else
            AStream.FStatus := -1;
        finally
          AStream.FLink.FLock.Release;
        end;
        AStream.FLink.Notify(AStream);
      end;
    end;
  finally
    vRequest.Free;
    vResponse.Free;
  end;
end;

procedure TRALNgHttp2Server.SetActive(const AValue: boolean);
var
  vInt, vCount: IntegerRAL;
begin
  if AValue = Active then
    Exit;
  if AValue then
  begin
    if not NGHTTP2Load(string(FLibraryName)) then
      raise ERALSocketError.Create(NGHTTP2LoadError);
    inherited SetActive(True);
    try
      if SSL.Enabled then
      begin
        FTLSContext := TRALTLSContext.Create(mtrServer);
        FTLSContext.SetMinVersion(SSL.MinVersion);
        FTLSContext.ALPN := NGHTTP2_PROTO_VERSION_ID;
        if SSL.CertPEM <> '' then
          FTLSContext.UseCertificatePEM(SSL.CertPEM, SSL.KeyPEM)
        else
          FTLSContext.UseCertificateFiles(SSL.CertFile, SSL.KeyFile, SSL.KeyPassword);
      end;
      FListener := TRALSocket.Create;
      FListener.Listen(IPConfig.IPv4Bind, Port);
      vCount := FPoolCount;
      if vCount <= 0 then
        vCount := TThread.ProcessorCount * 4;
      for vInt := 1 to vCount do
        FWorkers.Add(TRALNgHttp2Worker.Create(Self));
      FLoop := TRALSocketLoop.Create;
      FLoop.OnAccept := Accepted;
      if SSL.Enabled then
        FLoop.AddListener(FListener, 1)
      else
        FLoop.AddListener(FListener, 0);
      vCount := FPollThreads;
      if vCount <= 0 then
        vCount := TThread.ProcessorCount;
      FLoop.Start(vCount);
    except
      SetActive(False);
      raise;
    end;
  end
  else
  begin
    if FLoop <> nil then
    begin
      FLoop.Stop;
      FreeAndNil(FLoop);
    end;
    FreeAndNil(FListener);
    inherited SetActive(False);
    // the workers see Active False and end once the queue is empty
    for vInt := 0 to FWorkers.Count - 1 do
      TThread(FWorkers[vInt]).Terminate;
    FQueueEvent.SetEvent;
    for vInt := 0 to FWorkers.Count - 1 do
    begin
      TThread(FWorkers[vInt]).WaitFor;
      TThread(FWorkers[vInt]).Free;
    end;
    FWorkers.Clear;
    FQueueLock.Acquire;
    try
      for vInt := 0 to FQueue.Count - 1 do
        TRALNgHttp2Stream(FQueue[vInt]).Release;
      FQueue.Clear;
    finally
      FQueueLock.Release;
    end;
    FreeAndNil(FTLSContext);
  end;
end;

procedure TRALNgHttp2Server.SetSSL(const AValue: TRALNgHttp2SSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

function TRALNgHttp2Server.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
begin
  SSL.CertFile := ACertificate.CertificateFile;
  SSL.KeyFile := ACertificate.PrivateKeyFile;
  SSL.KeyPassword := ACertificate.PrivateKeyPassword;
  SSL.CertPEM := ACertificate.CertificatePEM;
  SSL.KeyPEM := ACertificate.PrivateKeyPEM;
  SSL.Enabled := True;
  if Active then
    Result := tarRestartNeeded
  else
    Result := tarApplied;
end;

class function TRALNgHttp2Server.SupportsLiveStream: boolean;
begin
  Result := True;
end;

function TRALNgHttp2Server.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

end.
