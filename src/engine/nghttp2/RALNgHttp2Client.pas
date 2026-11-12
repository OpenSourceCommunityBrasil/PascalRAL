{ The HTTP/2 client of RAL's own sockets: h2 over TLS (ALPN) and h2c with prior
  knowledge, framed by nghttp2. Connections are kept per scheme://host:port and
  certificate policy and, with ShareConnection, shared by every TRALClient of the
  process: many calls from many threads go as streams of one connection. A poll
  thread owns each connection and its session; a call waits on its own thread
  and reads its answer there, piece by piece when a stream handler wants it. }
unit RALNgHttp2Client;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALClient, RALRequest, RALResponse, RALParams,
  RALCustomObjects, RALStream, RALCompress, RALSocket, RALTLS, RALSocketLoop, nghttp2;

type
  { The nghttp2 engine of TRALClient (EngineType 'NgHttp2'): every call goes
    as HTTP/2, h2 for https:// and h2c (prior knowledge) for http://; a server
    that does not speak HTTP/2 fails the call. }
  TRALNgHttp2ClientHTTP = class(TRALClientHTTP)
  private
    /// The connection of this engine when ShareConnection is off.
    FOwnLink: TObject;
  public
    destructor Destroy; override;

    class function EngineName: StringRAL; override;
    class function EngineVersion: StringRAL; override;
    class function PackageDependency: StringRAL; override;
    procedure SendUrl(AURL: StringRAL; ARequest: TRALRequest; AResponse: TRALResponse;
      AMethod: TRALMethod); override;
    class function SupportsCertPin: boolean; override;
    class function SupportsHTTP2: boolean; override;
    class function SupportsMethod(AMethod: TRALMethod): boolean; override;
    class function SupportsSharedConnection: boolean; override;
  end;

var
  /// The nghttp2 library to load (empty: the usual names); set before the first call.
  RALNgHttp2ClientLibrary: string = '';

implementation

type
  TRALNgHttp2ClientConn = class;
  TRALNgHttp2ClientLink = class;

  /// One request in flight: what the poll thread fills and the caller reads.
  TRALNgHttp2ClientCall = class
  private
    /// The request body, read by nghttp2 on the poll thread; owned.
    FBody: TStream;
    /// The caller gave up (cancel or timeout): reset the stream.
    FCancelled: boolean;
    /// The pieces of the answer not yet taken by the caller.
    FChunks: TList;
    /// The stream ended (the answer is whole).
    FEnded: boolean;
    /// Why the stream or the connection failed; empty when it did not.
    FError: StringRAL;
    /// Wakes the caller.
    FEvent: TEvent;
    /// The request fields (name, value, name, value...).
    FFields: array of RawByteString;
    /// The answer fields (name: value lines) once the head arrived.
    FHeaders: StringRAL;
    /// The head arrived.
    FHeadReady: boolean;
    /// References: the caller and the connection while the stream is open.
    FRefCount: IntegerRAL;
    /// The status.
    FStatus: IntegerRAL;
    /// The stream id, 0 until submitted.
    FStreamId: Integer;
    /// The answer trailers (name=value).
    FTrailers: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    /// Takes a reference.
    procedure AddRef;
    /// Drops a reference; the last frees the call.
    procedure Release;
  end;

  { What a connection and its callers share, counted by reference: the lock, the
    calls to submit or reset, and the connection while it lives. }
  TRALNgHttp2ClientLink = class
  private
    /// Calls with a stream open (the connection's references).
    FActive: TList;
    /// The connection, nil once it closed.
    FConn: TRALNgHttp2ClientConn;
    /// The calls waiting to be submitted.
    FIncoming: TList;
    /// The pool key, empty for a private connection.
    FKey: StringRAL;
    /// Guards the link and the calls.
    FLock: TCriticalSection;
    /// References.
    FRefCount: IntegerRAL;
    /// The peer sent GOAWAY or the connection failed: no new calls.
    FRetired: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// Takes a reference.
    procedure AddRef;
    /// Fails every call with AError (connection lost); under the lock.
    procedure FailAll(const AError: StringRAL);
    /// Drops a reference; the last frees the link.
    procedure Release;
    /// Queues ACall for the connection; False when the connection is gone.
    function Submit(ACall: TRALNgHttp2ClientCall): boolean;
    /// Wakes the connection to reset a cancelled call.
    procedure Wake;
  end;

  /// The connection on a poll thread of the client loop.
  TRALNgHttp2ClientConn = class(TRALSocketLoopConnection)
  private
    /// The bytes received but not yet fed to nghttp2.
    FBacklog: TMemoryStream;
    /// The read position in FBacklog.
    FBacklogPos: IntegerRAL;
    /// The TLS context of the channel, nil for h2c; freed after the channel.
    FContext: TRALTLSContext;
    /// The link that queues the calls.
    FLink: TRALNgHttp2ClientLink;
    /// The nghttp2 client session.
    FSession: pnghttp2_session;

    /// Submits the queued calls and resets the cancelled ones.
    procedure DrainCalls;
    /// Writes what nghttp2 has; False when the link broke.
    function PumpOutput: boolean;
  protected
    procedure Closed; override;
  public
    /// The connection of ALink over AChannel; it owns the socket, channel and AContext.
    constructor Create(ALink: TRALNgHttp2ClientLink; ASocket: TRALSocket; AChannel: TRALChannel;
      AContext: TRALTLSContext);
    destructor Destroy; override;

    function FlushOutput: boolean; override;
    function HasOutput: boolean; override;
    procedure Serve(ANow: Int64RAL); override;
  end;

const
  /// Headers HTTP/2 forbids in a request.
  cHopByHop: array[0..6] of StringRAL = ('connection', 'keep-alive', 'proxy-connection',
    'transfer-encoding', 'upgrade', 'te', 'host');

var
  /// The poll threads of every client connection.
  gLoop: TRALSocketLoop = nil;
  /// Shared links by key.
  gLinks: TStringList = nil;
  /// Guards gLoop and gLinks.
  gLock: TCriticalSection = nil;

/// Whether AName may not go in an HTTP/2 request.
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

/// The client loop, started on first use.
function ClientLoop: TRALSocketLoop;
begin
  gLock.Acquire;
  try
    if gLoop = nil then
    begin
      gLoop := TRALSocketLoop.Create;
      gLoop.StartThreads(2);
    end;
    Result := gLoop;
  finally
    gLock.Release;
  end;
end;

/// Splits an http(s) URL; False when it is not one.
function SplitURL(const AURL: StringRAL; out ASecure: boolean; out AHost: StringRAL;
  out APort: IntegerRAL; out APath: StringRAL): boolean;
var
  vRest, vHostPort: StringRAL;
  vPos: IntegerRAL;
begin
  Result := False;
  ASecure := RALSameName(Copy(AURL, 1, 8), 'https://');
  if ASecure then
    vRest := Copy(AURL, 9, MaxInt)
  else if RALSameName(Copy(AURL, 1, 7), 'http://') then
    vRest := Copy(AURL, 8, MaxInt)
  else
    Exit;
  vPos := Pos('/', string(vRest));
  if vPos > 0 then
  begin
    vHostPort := Copy(vRest, 1, vPos - 1);
    APath := Copy(vRest, vPos, MaxInt);
  end
  else
  begin
    vHostPort := vRest;
    APath := '/';
  end;
  if ASecure then
    APort := 443
  else
    APort := 80;
  if (vHostPort <> '') and (vHostPort[1] = '[') then
  begin
    vPos := Pos(']', string(vHostPort));
    AHost := Copy(vHostPort, 2, vPos - 2);
    if Copy(vHostPort, vPos + 1, 1) = ':' then
      APort := StrToIntDef(string(Copy(vHostPort, vPos + 2, MaxInt)), APort);
  end
  else
  begin
    vPos := Pos(':', string(vHostPort));
    if vPos > 0 then
    begin
      AHost := Copy(vHostPort, 1, vPos - 1);
      APort := StrToIntDef(string(Copy(vHostPort, vPos + 1, MaxInt)), APort);
    end
    else
      AHost := vHostPort;
  end;
  Result := AHost <> '';
end;

/// The call of a stream, nil when none.
function CallOf(session: pnghttp2_session; stream_id: Integer): TRALNgHttp2ClientCall;
begin
  Result := TRALNgHttp2ClientCall(nghttp2_session_get_stream_user_data(session, stream_id));
end;

{ nghttp2 callbacks, on the poll thread; user_data is the connection }

/// Receives a header of a response.
function OnHeader(session: pnghttp2_session; frame: Pointer; name: PByte;
  namelen: nghttp2_size_t; value: PByte; valuelen: nghttp2_size_t; flags: Byte;
  user_data: Pointer): Integer; cdecl;
var
  vCall: TRALNgHttp2ClientCall;
  vName, vValue: StringRAL;
  vLink: TRALNgHttp2ClientLink;
begin
  Result := 0;
  vCall := CallOf(session, pnghttp2_frame_hd(frame)^.stream_id);
  if vCall = nil then
    Exit;
  SetString(vName, PAnsiChar(name), namelen);
  SetString(vValue, PAnsiChar(value), valuelen);
  vLink := TRALNgHttp2ClientConn(user_data).FLink;
  vLink.FLock.Acquire;
  try
    if pnghttp2_headers_frame(frame)^.cat = NGHTTP2_HCAT_RESPONSE then
    begin
      if vName = ':status' then
        vCall.FStatus := StrToIntDef(string(vValue), 0)
      else
        vCall.FHeaders := vCall.FHeaders + vName + ': ' + vValue + #13#10;
    end
    else if pnghttp2_headers_frame(frame)^.cat = NGHTTP2_HCAT_HEADERS then
    begin
      // trailers (an informational 1xx head is dropped)
      if vCall.FHeadReady and (vName <> '') and (vName[1] <> ':') then
      begin
        if vCall.FTrailers = nil then
          vCall.FTrailers := TStringList.Create;
        vCall.FTrailers.Add(string(vName) + '=' + string(vValue));
      end
      else if vName = ':status' then
        vCall.FStatus := StrToIntDef(string(vValue), 0);
    end;
  finally
    vLink.FLock.Release;
  end;
end;

/// Receives a chunk of a response body.
function OnDataChunk(session: pnghttp2_session; flags: Byte; stream_id: Integer;
  data: PByte; len: nghttp2_size_t; user_data: Pointer): Integer; cdecl;
var
  vCall: TRALNgHttp2ClientCall;
  vChunk: TMemoryStream;
  vLink: TRALNgHttp2ClientLink;
begin
  Result := 0;
  vCall := CallOf(session, stream_id);
  if vCall = nil then
    Exit;
  vChunk := TMemoryStream.Create;
  vChunk.WriteBuffer(data^, len);
  vLink := TRALNgHttp2ClientConn(user_data).FLink;
  vLink.FLock.Acquire;
  try
    vCall.FChunks.Add(vChunk);
  finally
    vLink.FLock.Release;
  end;
  vCall.FEvent.SetEvent;
end;

/// Handles a received frame.
function OnFrameRecv(session: pnghttp2_session; frame: Pointer;
  user_data: Pointer): Integer; cdecl;
var
  vHd: pnghttp2_frame_hd;
  vCall: TRALNgHttp2ClientCall;
  vLink: TRALNgHttp2ClientLink;
begin
  Result := 0;
  vHd := pnghttp2_frame_hd(frame);
  vLink := TRALNgHttp2ClientConn(user_data).FLink;
  if vHd^.type_ = NGHTTP2_GOAWAY then
  begin
    vLink.FLock.Acquire;
    try
      vLink.FRetired := True;
    finally
      vLink.FLock.Release;
    end;
    Exit;
  end;
  vCall := CallOf(session, vHd^.stream_id);
  if vCall = nil then
    Exit;
  vLink.FLock.Acquire;
  try
    if (vHd^.type_ = NGHTTP2_HEADERS) and
       (pnghttp2_headers_frame(frame)^.cat = NGHTTP2_HCAT_RESPONSE) and
       (vCall.FStatus >= 200) then
      vCall.FHeadReady := True;
    if (vHd^.type_ in [NGHTTP2_HEADERS, NGHTTP2_DATA]) and
       ((vHd^.flags and NGHTTP2_FLAG_END_STREAM) <> 0) then
      vCall.FEnded := True;
  finally
    vLink.FLock.Release;
  end;
  vCall.FEvent.SetEvent;
end;

/// Completes the call of a closed stream.
function OnStreamClose(session: pnghttp2_session; stream_id: Integer;
  error_code: Cardinal; user_data: Pointer): Integer; cdecl;
var
  vCall: TRALNgHttp2ClientCall;
  vLink: TRALNgHttp2ClientLink;
begin
  Result := 0;
  vCall := CallOf(session, stream_id);
  if vCall = nil then
    Exit;
  nghttp2_session_set_stream_user_data(session, stream_id, nil);
  vLink := TRALNgHttp2ClientConn(user_data).FLink;
  vLink.FLock.Acquire;
  try
    if not vCall.FEnded and (vCall.FError = '') then
      vCall.FError := StringRAL('stream reset: ' +
        string(AnsiString(nghttp2_http2_strerror(error_code))));
    vCall.FEnded := True;
    vLink.FActive.Remove(vCall);
  finally
    vLink.FLock.Release;
  end;
  vCall.FEvent.SetEvent;
  vCall.Release;
end;

/// Supplies the request body to nghttp2.
function OnBodyRead(session: pnghttp2_session; stream_id: Integer; buf: PByte;
  length: nghttp2_size_t; data_flags: PCardinal; source: pnghttp2_data_source;
  user_data: Pointer): nghttp2_ssize_t; cdecl;
var
  vCall: TRALNgHttp2ClientCall;
begin
  vCall := TRALNgHttp2ClientCall(source^.ptr);
  Result := vCall.FBody.Read(buf^, length);
  if Result <= 0 then
  begin
    Result := 0;
    data_flags^ := data_flags^ or NGHTTP2_DATA_FLAG_EOF;
  end;
end;

{ TRALNgHttp2ClientCall }

constructor TRALNgHttp2ClientCall.Create;
begin
  inherited Create;
  FChunks := TList.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FRefCount := 1;
end;

destructor TRALNgHttp2ClientCall.Destroy;
var
  vInt: IntegerRAL;
begin
  for vInt := 0 to FChunks.Count - 1 do
    TObject(FChunks[vInt]).Free;
  FChunks.Free;
  FEvent.Free;
  FBody.Free;
  FTrailers.Free;
  inherited Destroy;
end;

procedure TRALNgHttp2ClientCall.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

procedure TRALNgHttp2ClientCall.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

{ TRALNgHttp2ClientLink }

constructor TRALNgHttp2ClientLink.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FIncoming := TList.Create;
  FActive := TList.Create;
  FRefCount := 1;
end;

destructor TRALNgHttp2ClientLink.Destroy;
begin
  FIncoming.Free;
  FActive.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TRALNgHttp2ClientLink.AddRef;
begin
  RALAtomicInc(FRefCount);
end;

procedure TRALNgHttp2ClientLink.FailAll(const AError: StringRAL);
var
  vInt: IntegerRAL;
  vCall: TRALNgHttp2ClientCall;
begin
  for vInt := 0 to FIncoming.Count - 1 do
  begin
    vCall := TRALNgHttp2ClientCall(FIncoming[vInt]);
    vCall.FError := AError;
    vCall.FEnded := True;
    vCall.FEvent.SetEvent;
    vCall.Release;
  end;
  FIncoming.Clear;
  for vInt := 0 to FActive.Count - 1 do
  begin
    vCall := TRALNgHttp2ClientCall(FActive[vInt]);
    if vCall.FError = '' then
      vCall.FError := AError;
    vCall.FEnded := True;
    vCall.FEvent.SetEvent;
    vCall.Release;
  end;
  FActive.Clear;
end;

procedure TRALNgHttp2ClientLink.Release;
begin
  if RALAtomicDec(FRefCount) = 0 then
    Free;
end;

function TRALNgHttp2ClientLink.Submit(ACall: TRALNgHttp2ClientCall): boolean;
begin
  FLock.Acquire;
  try
    Result := (FConn <> nil) and not FRetired;
    if Result then
    begin
      ACall.AddRef;
      FIncoming.Add(ACall);
      FConn.Wake;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TRALNgHttp2ClientLink.Wake;
begin
  FLock.Acquire;
  try
    if FConn <> nil then
      FConn.Wake;
  finally
    FLock.Release;
  end;
end;

{ TRALNgHttp2ClientConn }

constructor TRALNgHttp2ClientConn.Create(ALink: TRALNgHttp2ClientLink;
  ASocket: TRALSocket; AChannel: TRALChannel; AContext: TRALTLSContext);
var
  vCallbacks: pnghttp2_session_callbacks;
  vOption: pnghttp2_option;
  vSettings: array[0..1] of nghttp2_settings_entry;
  vRes: Integer;
begin
  inherited Create;
  FLink := ALink;
  FLink.AddRef;
  FLink.FConn := Self;
  FSocket := ASocket;
  FChannel := AChannel;
  FContext := AContext;
  FBacklog := TMemoryStream.Create;
  nghttp2_session_callbacks_new(vCallbacks);
  try
    nghttp2_session_callbacks_set_on_header_callback(vCallbacks, OnHeader);
    nghttp2_session_callbacks_set_on_data_chunk_recv_callback(vCallbacks, OnDataChunk);
    nghttp2_session_callbacks_set_on_frame_recv_callback(vCallbacks, OnFrameRecv);
    nghttp2_session_callbacks_set_on_stream_close_callback(vCallbacks, OnStreamClose);
    nghttp2_option_new(vOption);
    try
      vRes := nghttp2_session_client_new2(FSession, vCallbacks, Self, vOption);
    finally
      nghttp2_option_del(vOption);
    end;
  finally
    nghttp2_session_callbacks_del(vCallbacks);
  end;
  if vRes <> 0 then
    raise ERALSocketError.Create('nghttp2: ' + string(AnsiString(nghttp2_strerror(vRes))));
  vSettings[0].settings_id := NGHTTP2_SETTINGS_ENABLE_PUSH;
  vSettings[0].value := 0;
  vSettings[1].settings_id := NGHTTP2_SETTINGS_INITIAL_WINDOW_SIZE;
  vSettings[1].value := 1048576;
  nghttp2_submit_settings(FSession, NGHTTP2_FLAG_NONE, @vSettings[0], 2);
  nghttp2_session_set_local_window_size(FSession, NGHTTP2_FLAG_NONE, 0, 16 * 1048576);
end;

destructor TRALNgHttp2ClientConn.Destroy;
begin
  if FSession <> nil then
    nghttp2_session_del(FSession);
  FBacklog.Free;
  FChannel.Free;
  FContext.Free;
  FSocket.Free;
  FLink.Release;
  inherited Destroy;
end;

procedure TRALNgHttp2ClientConn.Closed;
var
  vIndex: Integer;
begin
  FLink.FLock.Acquire;
  try
    FLink.FConn := nil;
    FLink.FRetired := True;
    FLink.FailAll('the connection closed');
  finally
    FLink.FLock.Release;
  end;
  if FLink.FKey <> '' then
  begin
    gLock.Acquire;
    try
      if gLinks.Find(string(FLink.FKey), vIndex) and (gLinks.Objects[vIndex] = FLink) then
      begin
        gLinks.Delete(vIndex);
        FLink.Release;
      end;
    finally
      gLock.Release;
    end;
  end;
  inherited Closed;
end;

procedure TRALNgHttp2ClientConn.DrainCalls;
var
  vCalls: TList;
  vInt, vField, vCount: IntegerRAL;
  vCall: TRALNgHttp2ClientCall;
  vNv: array of nghttp2_nv;
  vProvider: nghttp2_data_provider;
  vId: Integer;
begin
  vCalls := TList.Create;
  try
    FLink.FLock.Acquire;
    try
      vCalls.Assign(FLink.FIncoming);
      FLink.FIncoming.Clear;
      // the ones whose caller gave up: reset
      for vInt := 0 to FLink.FActive.Count - 1 do
      begin
        vCall := TRALNgHttp2ClientCall(FLink.FActive[vInt]);
        if vCall.FCancelled and (vCall.FStreamId > 0) and not vCall.FEnded then
        begin
          nghttp2_submit_rst_stream(FSession, 0, vCall.FStreamId, NGHTTP2_CANCEL);
          vCall.FStreamId := -vCall.FStreamId;
        end;
      end;
    finally
      FLink.FLock.Release;
    end;
    for vInt := 0 to vCalls.Count - 1 do
    begin
      vCall := TRALNgHttp2ClientCall(vCalls[vInt]);
      if vCall.FCancelled then
      begin
        vCall.FEnded := True;
        vCall.FEvent.SetEvent;
        vCall.Release;
        Continue;
      end;
      vCount := Length(vCall.FFields) div 2;
      SetLength(vNv, vCount);
      for vField := 0 to vCount - 1 do
      begin
        vNv[vField].name := PByte(Pointer(vCall.FFields[vField * 2]));
        vNv[vField].namelen := Length(vCall.FFields[vField * 2]);
        vNv[vField].value := PByte(Pointer(vCall.FFields[vField * 2 + 1]));
        vNv[vField].valuelen := Length(vCall.FFields[vField * 2 + 1]);
        vNv[vField].flags := NGHTTP2_NV_FLAG_NONE;
      end;
      if vCall.FBody <> nil then
      begin
        vProvider.source.ptr := vCall;
        vProvider.read_callback := OnBodyRead;
        vId := nghttp2_submit_request(FSession, nil, @vNv[0], vCount, @vProvider, vCall);
      end
      else
        vId := nghttp2_submit_request(FSession, nil, @vNv[0], vCount, nil, vCall);
      FLink.FLock.Acquire;
      try
        if vId < 0 then
        begin
          vCall.FError := StringRAL('nghttp2: ' + string(AnsiString(nghttp2_strerror(vId))));
          vCall.FEnded := True;
          vCall.FEvent.SetEvent;
          vCall.Release;
        end
        else
        begin
          vCall.FStreamId := vId;
          FLink.FActive.Add(vCall);
        end;
      finally
        FLink.FLock.Release;
      end;
    end;
  finally
    vCalls.Free;
  end;
end;

function TRALNgHttp2ClientConn.FlushOutput: boolean;
begin
  Result := PumpOutput;
end;

function TRALNgHttp2ClientConn.HasOutput: boolean;
begin
  Result := (FBacklog.Size > FBacklogPos) or
    ((FSession <> nil) and (nghttp2_session_want_write(FSession) <> 0));
end;

function TRALNgHttp2ClientConn.PumpOutput: boolean;
var
  vData: PByte;
  vLen: nghttp2_ssize_t;
  vWritten: IntegerRAL;
begin
  Result := True;
  while FBacklog.Size > FBacklogPos do
  begin
    vWritten := FChannel.Write(PByte(NativeUInt(FBacklog.Memory) + NativeUInt(FBacklogPos))^,
      FBacklog.Size - FBacklogPos);
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
      FBacklog.WriteBuffer(PByte(NativeUInt(vData) + NativeUInt(vWritten))^, vLen - vWritten);
      Break;
    end;
  end;
  FChannel.Flush;
end;

procedure TRALNgHttp2ClientConn.Serve(ANow: Int64RAL);
var
  vBuffer: array[0..32767] of Byte;
  vRead: IntegerRAL;
begin
  if (Returned and RALPollErr) <> 0 then
  begin
    Close;
    Exit;
  end;
  try
    repeat
      vRead := FChannel.Read(vBuffer, SizeOf(vBuffer));
      if vRead = 0 then
      begin
        Close;
        Exit;
      end;
      if (vRead > 0) and (nghttp2_session_mem_recv(FSession, @vBuffer[0], vRead) < 0) then
      begin
        Close;
        Exit;
      end;
    until (vRead <> SizeOf(vBuffer)) and not FChannel.HasBuffered;
  except
    Close;
    Exit;
  end;
  DrainCalls;
  if not PumpOutput then
  begin
    Close;
    Exit;
  end;
  if (nghttp2_session_want_read(FSession) = 0) and (nghttp2_session_want_write(FSession) = 0) then
    Close;
end;

{ TRALNgHttp2ClientHTTP }

destructor TRALNgHttp2ClientHTTP.Destroy;
begin
  if FOwnLink <> nil then
  begin
    TRALNgHttp2ClientLink(FOwnLink).FLock.Acquire;
    try
      if TRALNgHttp2ClientLink(FOwnLink).FConn <> nil then
        TRALNgHttp2ClientLink(FOwnLink).FConn.Close;
    finally
      TRALNgHttp2ClientLink(FOwnLink).FLock.Release;
    end;
    TRALNgHttp2ClientLink(FOwnLink).Wake;
    TRALNgHttp2ClientLink(FOwnLink).Release;
  end;
  inherited Destroy;
end;

class function TRALNgHttp2ClientHTTP.EngineName: StringRAL;
begin
  Result := 'NgHttp2';
end;

class function TRALNgHttp2ClientHTTP.EngineVersion: StringRAL;
begin
  Result := StringRAL(NGHTTP2VersionStr);
end;

class function TRALNgHttp2ClientHTTP.PackageDependency: StringRAL;
begin
  Result := 'NgHttp2RAL';
end;

procedure TRALNgHttp2ClientHTTP.SendUrl(AURL: StringRAL; ARequest: TRALRequest;
  AResponse: TRALResponse; AMethod: TRALMethod);
var
  vSecure: boolean;
  vHost, vPath, vKey, vAuthority, vHeaders, vCookies, vName: StringRAL;
  vPort, vInt, vIndex: IntegerRAL;
  vLink: TRALNgHttp2ClientLink;
  vCall: TRALNgHttp2ClientCall;
  vSocket: TRALSocket;
  vChannel: TRALChannel;
  vTLS: TRALTLSChannel;
  vContext: TRALTLSContext;
  vCert: TRALCertInfo;
  vDeadline: Int64RAL;
  vBody: TRALBodyStream;
  vSink: TRALClientStreamSink;
  vTarget: TStream;
  vChunks: TList;
  vEnded, vHead, vStale: boolean;
  vScheme: string;
  vError: StringRAL;
  vWait: Cardinal;
  vParam: TRALParam;

  procedure AddField(const AName, AValue: StringRAL);
  begin
    SetLength(vCall.FFields, Length(vCall.FFields) + 2);
    vCall.FFields[High(vCall.FFields) - 1] := RawByteString(LowerCase(string(AName)));
    vCall.FFields[High(vCall.FFields)] := RawByteString(AValue);
  end;

begin
  AResponse.Clear;
  AResponse.AddHeader('RALEngine', 'NgHttp2');
  if not NGHTTP2Load(RALNgHttp2ClientLibrary) then
  begin
    SetTransportError(AResponse, rteOther, -1, StringRAL(NGHTTP2LoadError));
    Exit;
  end;
  if not SplitURL(AURL, vSecure, vHost, vPort, vPath) then
  begin
    SetTransportError(AResponse, rteOther, -1, 'not an http(s) URL: ' + AURL);
    Exit;
  end;
  // the connection: shared by key, or this engine's own
  if vSecure then
    vScheme := 'https'
  else
    vScheme := 'http';
  vKey := StringRAL(Format('%s://%s:%d|%s', [vScheme,
    string(vHost), vPort, string(CertPolicyKey)]));
  vLink := nil;
  if Parent.ShareConnection then
  begin
    gLock.Acquire;
    try
      if gLinks.Find(string(vKey), vIndex) then
      begin
        vLink := TRALNgHttp2ClientLink(gLinks.Objects[vIndex]);
        vLink.AddRef;
      end;
    finally
      gLock.Release;
    end;
  end
  else if FOwnLink <> nil then
  begin
    if TRALNgHttp2ClientLink(FOwnLink).FKey = '' then
    begin
      vLink := TRALNgHttp2ClientLink(FOwnLink);
      vLink.AddRef;
    end;
  end;
  if vLink <> nil then
  begin
    vLink.FLock.Acquire;
    try
      vStale := (vLink.FConn = nil) or vLink.FRetired;
    finally
      vLink.FLock.Release;
    end;
    if vStale then
    begin
      vLink.Release;
      vLink := nil;
    end;
  end;
  if vLink = nil then
  begin
    vDeadline := RALSocketNow + Parent.ConnectTimeout;
    vSocket := TRALSocket.Create;
    vChannel := nil;
    vContext := nil;
    try
      try
        vSocket.Connect(vHost, vPort, Parent.ConnectTimeout);
        vChannel := TRALSocketChannel.Create(vSocket, False);
        if vSecure then
        begin
          vContext := TRALTLSContext.Create(mtrClient);
          vContext.ALPN := NGHTTP2_PROTO_VERSION_ID;
          vContext.LoadSystemCAs;
          vTLS := TRALTLSChannel.Create(vChannel, vContext);
          vChannel := vTLS;
          vTLS.Handshake(vHost, vDeadline - RALSocketNow);
          vCert := vTLS.PeerCertificate;
          if CertCheckWanted then
          begin
            if not AcceptServerCert(vCert) then
            begin
              SetTransportError(AResponse, rteCertificate, -1, 'certificate refused');
              Exit;
            end;
          end
          else if (Parent.SSL.Verify <> svNever) and not vCert.Trusted then
          begin
            SetTransportError(AResponse, rteCertificate, -1,
              'certificate not trusted: ' + vCert.Error);
            Exit;
          end;
          if vTLS.SelectedALPN <> NGHTTP2_PROTO_VERSION_ID then
          begin
            SetTransportError(AResponse, rteConnect, -1, 'the server does not speak HTTP/2 (ALPN)');
            Exit;
          end;
        end;
        vLink := TRALNgHttp2ClientLink.Create;
        TRALNgHttp2ClientConn.Create(vLink, vSocket, vChannel, vContext);
        vSocket := nil;
        vChannel := nil;
        vContext := nil;
      except
        on E: Exception do
        begin
          SetTransportError(AResponse, rteConnect, -1, StringRAL(E.Message));
          Exit;
        end;
      end;
    finally
      if vChannel <> nil then
        vChannel.Free;
      vSocket.Free;
      vContext.Free;
    end;
    if Parent.ShareConnection then
    begin
      vLink.FKey := vKey;
      gLock.Acquire;
      try
        if gLinks.Find(string(vKey), vIndex) then
          gLinks.Delete(vIndex);
        vLink.AddRef;
        gLinks.AddObject(string(vKey), vLink);
      finally
        gLock.Release;
      end;
    end
    else
    begin
      if FOwnLink <> nil then
        TRALNgHttp2ClientLink(FOwnLink).Release;
      vLink.AddRef;
      FOwnLink := vLink;
    end;
    ClientLoop.Adopt(vLink.FConn);
  end;
  // the request
  vCall := TRALNgHttp2ClientCall.Create;
  vBody := nil;
  vSink := nil;
  try
    vAuthority := vHost;
    if Pos(':', string(vAuthority)) > 0 then
      vAuthority := '[' + vAuthority + ']';
    if not ((vSecure and (vPort = 443)) or ((not vSecure) and (vPort = 80))) then
      vAuthority := vAuthority + ':' + StringRAL(IntToStr(vPort));
    AddField(':method', RALMethodToHTTPMethod(AMethod));
    if vSecure then
      AddField(':scheme', 'https')
    else
      AddField(':scheme', 'http');
    AddField(':authority', vAuthority);
    AddField(':path', vPath);
    ARequest.Params.AddParam('User-Agent', Parent.UserAgent, rpkHEADER);
    ARequest.ContentCompress := Parent.CompressType;
    ARequest.Params.AddParam('Accept-Encoding', AcceptEncodingFor(ARequest), rpkHEADER);
    ARequest.CriptoKey := Parent.CriptoOptions.Key;
    ARequest.ContentCripto := Parent.CriptoOptions.CriptType;
    if Parent.CriptoOptions.CriptType <> crNone then
    begin
      ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
      ARequest.Params.AddParam('Accept-Encription', SupportedEncriptKind, rpkHEADER);
    end;
    vCall.FBody := ARequest.TakeWireStream;
    if vCall.FBody <> nil then
      vCall.FBody.Position := 0;
    if ARequest.ContentType <> '' then
      AddField('content-type', ARequest.ContentType);
    if ARequest.ContentDisposition <> '' then
      AddField('content-disposition', ARequest.ContentDisposition);
    if (ARequest.ContentCompress <> ctNone) and (ARequest.ContentEncoding <> '') then
      AddField('content-encoding', ARequest.ContentEncoding);
    if vCall.FBody <> nil then
      AddField('content-length', StringRAL(IntToStr(vCall.FBody.Size)));
    for vInt := 0 to ARequest.Params.Count - 1 do
    begin
      vParam := ARequest.Params.Index[vInt];
      vName := vParam.ParamName;
      if (vParam.Kind = rpkHEADER) and (vName <> '') and not IsHopByHop(vName) and
         not RALSameName(vName, 'content-type') and not RALSameName(vName, 'content-length') then
        AddField(vName, RALSafeHeaderText(vParam.AsString));
    end;
    vCookies := ARequest.Params.CookieHeaderText;
    if vCookies <> '' then
      AddField('cookie', vCookies);
    if StreamWanted then
    begin
      vBody := TRALBodyStream.Create(-1, Parent.SpoolAbove);
      vSink := NewResponseSink(AResponse, vBody);
      vTarget := vSink;
    end
    else
    begin
      vBody := TRALBodyStream.Create(-1, Parent.SpoolAbove);
      vTarget := vBody;
    end;
    if not vLink.Submit(vCall) then
    begin
      SetTransportError(AResponse, rteConnect, -1, 'the connection closed');
      Exit;
    end;
    // wait for the answer, piece by piece, on this thread
    if Parent.RequestTimeout > 0 then
      vWait := Parent.RequestTimeout
    else
      vWait := INFINITE;
    vHead := False;
    vChunks := TList.Create;
    try
      repeat
        if vCall.FEvent.WaitFor(vWait) <> wrSignaled then
        begin
          vLink.FLock.Acquire;
          try
            vCall.FCancelled := True;
          finally
            vLink.FLock.Release;
          end;
          vLink.Wake;
          SetTransportError(AResponse, rteTimeout, 10060, 'no answer within RequestTimeout');
          Exit;
        end;
        vLink.FLock.Acquire;
        try
          vChunks.Assign(vCall.FChunks);
          vCall.FChunks.Clear;
          vEnded := vCall.FEnded;
          vError := vCall.FError;
          if not vHead and vCall.FHeadReady then
          begin
            vHead := True;
            vHeaders := vCall.FHeaders;
          end;
        finally
          vLink.FLock.Release;
        end;
        if vHead and (vHeaders <> '') then
        begin
          AResponse.StatusCode := vCall.FStatus;
          AResponse.Params.AppendParamsListText(vHeaders, rpkHEADER);
          AResponse.ContentType := AResponse.ParamByName('content-type').AsString;
          AResponse.ContentEncoding := AResponse.ParamByName('content-encoding').AsString;
          AResponse.Params.CompressType := AResponse.ContentCompress;
          AResponse.ContentEncription := AResponse.ParamByName('content-encription').AsString;
          AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
          AResponse.Params.CriptoOptions.Key := Parent.CriptoOptions.Key;
          AResponse.ContentDisposition := AResponse.ParamByName('content-disposition').AsString;
          AResponse.ProtocolVersion := rhv2;
          vHeaders := '';
        end;
        try
          for vInt := 0 to vChunks.Count - 1 do
          begin
            TMemoryStream(vChunks[vInt]).Position := 0;
            if TMemoryStream(vChunks[vInt]).Size > 0 then
              vTarget.WriteBuffer(TMemoryStream(vChunks[vInt]).Memory^,
                TMemoryStream(vChunks[vInt]).Size);
          end;
        except
          on E: ERALStreamCancelled do
          begin
            vLink.FLock.Acquire;
            try
              vCall.FCancelled := True;
            finally
              vLink.FLock.Release;
            end;
            vLink.Wake;
            SetTransportError(AResponse, rteCancelled, -1, StringRAL(E.Message));
            Exit;
          end;
        end;
        for vInt := 0 to vChunks.Count - 1 do
          TObject(vChunks[vInt]).Free;
        vChunks.Clear;
      until vEnded;
    finally
      for vInt := 0 to vChunks.Count - 1 do
        TObject(vChunks[vInt]).Free;
      vChunks.Free;
    end;
    if vError <> '' then
    begin
      if vHead then
        SetTransportError(AResponse, rteOther, -1, vError)
      else
        SetTransportError(AResponse, rteConnect, -1, vError);
      Exit;
    end;
    if not vHead then
    begin
      SetTransportError(AResponse, rteOther, -1, 'the stream ended without an answer');
      Exit;
    end;
    if vCall.FTrailers <> nil then
      for vInt := 0 to vCall.FTrailers.Count - 1 do
        AResponse.AddTrailer(StringRAL(vCall.FTrailers.Names[vInt]),
          StringRAL(vCall.FTrailers.ValueFromIndex[vInt]));
    if vSink <> nil then
    begin
      vSink.Finish;
      AResponse.SetWireBody(vBody.Detach, boOwned);
    end
    else
      AResponse.SetWireBody(vBody.Detach, boOwned);
  finally
    FreeAndNil(vSink);
    FreeAndNil(vBody);
    vCall.Release;
    vLink.Release;
  end;
end;

class function TRALNgHttp2ClientHTTP.SupportsCertPin: boolean;
begin
  Result := True;
end;

class function TRALNgHttp2ClientHTTP.SupportsHTTP2: boolean;
begin
  Result := True;
end;

class function TRALNgHttp2ClientHTTP.SupportsMethod(AMethod: TRALMethod): boolean;
begin
  Result := (AMethod > amALL) and (AMethod < amUNKNOWN);
end;

class function TRALNgHttp2ClientHTTP.SupportsSharedConnection: boolean;
begin
  Result := True;
end;

initialization
  gLock := TCriticalSection.Create;
  gLinks := TStringList.Create;
  gLinks.Sorted := True;
  gLinks.CaseSensitive := True;
  RegisterClass(TRALNgHttp2ClientHTTP);
  RegisterEngine(TRALNgHttp2ClientHTTP);

finalization
  UnregisterEngine(TRALNgHttp2ClientHTTP);
  if gLoop <> nil then
  begin
    gLoop.Stop;
    FreeAndNil(gLoop);
  end;
  while gLinks.Count > 0 do
  begin
    TRALNgHttp2ClientLink(gLinks.Objects[0]).Release;
    gLinks.Delete(0);
  end;
  FreeAndNil(gLinks);
  FreeAndNil(gLock);

end.
