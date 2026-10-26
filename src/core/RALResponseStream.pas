/// A response sent in pieces while the handler runs (TRALResponse.Send): the channel
/// that frames and serialises the pieces, and the writer each engine implements.
unit RALResponseStream;

{$I PascalRAL.inc}
{$IFDEF FPC}
  {$INTERFACES COM}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALCustomObjects, RALSSE, RALCompress;

type
  /// Why a stream ended: as asked, the client left, a send timed out, the server stopped, or an error.
  TRALStreamCloseReason = (srEnded, srClientGone, srTimeout, srServerStopped, srError);

  { A response stream that outlives its handler (TRALResponse.Detach). Counted by
    reference: it lives while anyone holds it, and a Send on a closed stream answers
    False instead of writing to freed memory. Any thread may call it }
  IRALResponseStream = interface
    ['{6F1C2B44-7A3D-4E59-9C1B-2D7E5A8F0B31}']
    /// Ends the body as asked (the last chunk). Does nothing when it is closed already.
    procedure Close;
    /// True once it is closed, by Close, the client, a timeout or the server.
    function Closed: boolean;
    /// The best answer the engine can give on whether the client is still there.
    function Connected: boolean;
    /// True when it carries text/event-stream.
    function IsEventStream: boolean;
    /// Sends AText now; False when the stream is closed or the client is gone.
    function Send(const AText: StringRAL): boolean;
    /// Sends ASize bytes now; False when the stream is closed or the client is gone.
    function SendBuffer(const ABuffer; ASize: IntegerRAL): boolean;
    /// Sends an SSE comment line (': text'), which keeps a quiet stream alive.
    function SendComment(const AText: StringRAL = 'ping'): boolean;
    /// Sends one Server-Sent Event (RALSSEEvent).
    function SendEvent(const AData: StringRAL; const AEvent: StringRAL = '';
      const AId: StringRAL = ''; ARetry: IntegerRAL = -1): boolean;
  end;

  { TRALStreamWriter }

  { What an engine implements to put a stream on its connection: the status line
    and headers once, then raw bytes, then the end. The channel frames the pieces
    (chunked) unless FramesItself; it calls the writer under its own lock, from the
    thread that sends }
  TRALStreamWriter = class
  private
    FChunked: boolean;
    FFramesItself: boolean;
    FHeadOnly: boolean;
    FSendTimeout: IntegerRAL;
  public
    { True while the connection can still be written to; asked when nothing is
      being sent. The default cannot tell and answers True }
    function Alive: boolean; virtual;
    /// Sends what WriteRaw buffered, when the engine buffers.
    procedure Flush; virtual;
    { The status line and the headers of AResponse (a TRALResponse); Chunked says
      whether the body goes chunked or delimited by the end of the connection }
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); virtual; abstract;
    { The end of the body. AAborted is a failure: the connection closes without the
      last chunk, the sign HTTP has for a truncated body }
    procedure WriteEnd(AAborted: boolean); virtual;
    /// Writes ASize bytes as they are; False when the connection failed.
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; virtual; abstract;

    /// The body goes as Transfer-Encoding: chunked (HTTP/1.1); False, delimited by close (HTTP/1.0).
    property Chunked: boolean read FChunked write FChunked;
    /// The library frames the body itself (HTTP/2, a pull engine): the channel writes the bare data.
    property FramesItself: boolean read FFramesItself write FFramesItself;
    /// A HEAD request: the headers alone, the pieces are not sent.
    property HeadOnly: boolean read FHeadOnly write FHeadOnly;
    /// Milliseconds a write may block (TRALServer.StreamSendTimeout); 0 is no limit.
    property SendTimeout: IntegerRAL read FSendTimeout write FSendTimeout;
  end;

  TRALResponseStream = class;

  /// Fired once when a stream closes, with why.
  TRALStreamClosedEvent = procedure(AStream: TRALResponseStream;
    AReason: TRALStreamCloseReason) of object;

  { TRALResponseStream }

  { The channel of one streamed response: its state, the lock that serialises the
    pieces, the chunked framing and the engine's writer. It keeps no request nor
    response - everything it needs became bytes at the commit - so it is safe to
    hold after the engine freed them }
  TRALResponseStream = class(TInterfacedObject, IRALResponseStream)
  private
    FBytes: Int64RAL;
    FClosed: boolean;
    FClosedEvent: TEvent;
    /// What the encoder produced for the piece being sent.
    FEncoded: TMemoryStream;
    FEncoder: TRALStreamCoder;
    FEventStream: boolean;
    FLock: TCriticalSection;
    FOnClosed: TRALStreamClosedEvent;
    FReason: TRALStreamCloseReason;
    FWriter: TRALStreamWriter;

    /// Ends the stream under the lock, with AReason; AAborted skips the last chunk.
    procedure Finish(AReason: TRALStreamCloseReason; AAborted: boolean);
    /// Writes one piece as it goes on the wire, framed; under the lock.
    function WritePiece(const ABuffer; ASize: IntegerRAL): boolean;
  public
    { A channel over AWriter, which it owns from here; AEventStream when the body is
      text/event-stream }
    constructor Create(AWriter: TRALStreamWriter; AEventStream: boolean);
    destructor Destroy; override;

    { Ends the stream as a failure: no last chunk, the connection closes. Does
      nothing when it is closed already }
    procedure Abort(AReason: TRALStreamCloseReason = srError);
    procedure Close;
    function Closed: boolean;
    function Connected: boolean;
    function IsEventStream: boolean;
    function Send(const AText: StringRAL): boolean;
    function SendBuffer(const ABuffer; ASize: IntegerRAL): boolean;
    function SendComment(const AText: StringRAL = 'ping'): boolean;
    function SendEvent(const AData: StringRAL; const AEvent: StringRAL = '';
      const AId: StringRAL = ''; ARetry: IntegerRAL = -1): boolean;
    { Sends AStream from its start in blocks; False when the stream is closed or the
      client is gone. AStream stays the caller's }
    function SendStream(AStream: TStream): boolean;
    { Compresses every piece from here with AEncoder (gzip or deflate, flushed per
      piece), which the channel owns; set by the commit, before the first piece }
    procedure SetEncoder(AEncoder: TRALStreamCoder);
    { Waits up to ATimeout milliseconds for the stream to close; True once it is
      closed. INFINITE waits for good }
    function WaitClosed(ATimeout: Cardinal): boolean;

    /// Bytes of body sent so far, framing not counted.
    property Bytes: Int64RAL read FBytes;
    /// Fired once when it closes; TRALServer uses it to keep its list of open streams.
    property OnClosed: TRALStreamClosedEvent read FOnClosed write FOnClosed;
    /// Why it closed; meaningful once Closed.
    property Reason: TRALStreamCloseReason read FReason;
  end;

  /// A response committed by Send can no longer have its status, headers or body changed.
  ERALResponseCommitted = class(Exception);
  /// A stream refused: the RAL cipher of the body has no piece a client can read.
  ERALStreamCipher = class(Exception);
  /// A stream refused because TRALConcurrencyPlugin.MaxStreams streams are open.
  ERALStreamLimit = class(Exception);

{ The chunked framing of one piece: its size in hex, CRLF, the bytes, CRLF. ASize
  must be above 0 - a chunk of size 0 ends the body }
function RALChunkHeader(ASize: IntegerRAL): RawByteString;
/// The end of a chunked body: the chunk of size 0 and the blank line (no trailers).
function RALChunkTrailer: RawByteString;

implementation

function RALChunkHeader(ASize: IntegerRAL): RawByteString;
begin
  Result := RawByteString(IntToHex(ASize, 1)) + #13#10;
end;

function RALChunkTrailer: RawByteString;
begin
  Result := '0'#13#10#13#10;
end;

{ TRALStreamWriter }

function TRALStreamWriter.Alive: boolean;
begin
  Result := True;
end;

procedure TRALStreamWriter.Flush;
begin
  // an engine that buffers WriteRaw sends here
end;

procedure TRALStreamWriter.WriteEnd(AAborted: boolean);
begin
  // the engine closes or keeps its connection here
end;

{ TRALResponseStream }

constructor TRALResponseStream.Create(AWriter: TRALStreamWriter; AEventStream: boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FEventStream := AEventStream;
  FLock := TCriticalSection.Create;
  FClosedEvent := TEvent.Create(nil, True, False, '');
end;

destructor TRALResponseStream.Destroy;
begin
  // the last holder let go of a stream nobody closed: it ends as asked
  Finish(srEnded, False);
  FreeAndNil(FEncoder);
  FreeAndNil(FEncoded);
  FreeAndNil(FClosedEvent);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALResponseStream.Finish(AReason: TRALStreamCloseReason; AAborted: boolean);
var
  vTrailer: RawByteString;
  vNotify: boolean;
begin
  vNotify := False;
  FLock.Enter;
  try
    if FClosed then
      Exit;
    FClosed := True;
    FReason := AReason;
    if FWriter <> nil then
    begin
      try
        // what the compressor still holds, and its trailer, before the last chunk
        if (not AAborted) and (FEncoder <> nil) and (not FWriter.HeadOnly) then
        begin
          FEncoded.Position := 0;
          FEncoder.Finish(FEncoded);
          if FEncoded.Position > 0 then
            WritePiece(FEncoded.Memory^, FEncoded.Position);
        end;
        if (not AAborted) and FWriter.Chunked and (not FWriter.FramesItself) and
           (not FWriter.HeadOnly) then
        begin
          vTrailer := RALChunkTrailer;
          FWriter.WriteRaw(Pointer(vTrailer)^, Length(vTrailer));
        end;
        if not AAborted then
          FWriter.Flush;
        FWriter.WriteEnd(AAborted);
      except
        // the connection is going away; what matters is that the stream is closed
      end;
      FreeAndNil(FWriter);
    end;
    vNotify := True;
  finally
    FLock.Leave;
  end;
  { the server hears first, and only then is whoever waits woken: an engine that
    wakes frees its response and with it its hold on this object, which must be
    out of the server's list by then }
  if vNotify and Assigned(FOnClosed) then
    try
      FOnClosed(Self, AReason);
    except
      // a listener's failure does not reopen the stream
    end;
  FClosedEvent.SetEvent;
end;

procedure TRALResponseStream.Abort(AReason: TRALStreamCloseReason);
begin
  Finish(AReason, True);
end;

procedure TRALResponseStream.Close;
begin
  Finish(srEnded, False);
end;

function TRALResponseStream.Closed: boolean;
begin
  FLock.Enter;
  try
    Result := FClosed;
  finally
    FLock.Leave;
  end;
end;

function TRALResponseStream.Connected: boolean;
var
  vAlive: boolean;
begin
  FLock.Enter;
  try
    vAlive := (not FClosed) and (FWriter <> nil) and FWriter.Alive;
  finally
    FLock.Leave;
  end;
  if (not vAlive) and (not Closed) then
    Abort(srClientGone);
  Result := vAlive;
end;

function TRALResponseStream.IsEventStream: boolean;
begin
  Result := FEventStream;
end;

function TRALResponseStream.WritePiece(const ABuffer; ASize: IntegerRAL): boolean;
var
  vHead: RawByteString;
begin
  if FWriter.Chunked and not FWriter.FramesItself then
  begin
    vHead := RALChunkHeader(ASize);
    Result := FWriter.WriteRaw(Pointer(vHead)^, Length(vHead)) and
      FWriter.WriteRaw(ABuffer, ASize);
    if Result then
    begin
      vHead := #13#10;
      Result := FWriter.WriteRaw(Pointer(vHead)^, 2);
    end;
  end
  else
    Result := FWriter.WriteRaw(ABuffer, ASize);
end;

procedure TRALResponseStream.SetEncoder(AEncoder: TRALStreamCoder);
begin
  FLock.Enter;
  try
    FreeAndNil(FEncoder);
    FEncoder := AEncoder;
    if (FEncoder <> nil) and (FEncoded = nil) then
      FEncoded := TMemoryStream.Create;
  finally
    FLock.Leave;
  end;
end;

function TRALResponseStream.SendBuffer(const ABuffer; ASize: IntegerRAL): boolean;
var
  vFailed: boolean;
begin
  Result := False;
  vFailed := False;
  FLock.Enter;
  try
    if FClosed or (FWriter = nil) then
      Exit;
    if FWriter.HeadOnly or (ASize <= 0) then
    begin
      Result := True;
      Exit;
    end;
    if FEncoder <> nil then
    begin
      // compressed and flushed: the piece leaves whole, even when it grew
      FEncoded.Position := 0;
      FEncoder.Update(ABuffer, ASize, FEncoded);
      Result := (FEncoded.Position = 0) or
        WritePiece(FEncoded.Memory^, FEncoded.Position);
    end
    else
      Result := WritePiece(ABuffer, ASize);
    if Result then
    begin
      FWriter.Flush;
      Inc(FBytes, ASize);
    end
    else
      vFailed := True;
  finally
    FLock.Leave;
  end;
  // a write that failed is the client gone (or a send that timed out)
  if vFailed then
    Abort(srClientGone);
end;

function TRALResponseStream.Send(const AText: StringRAL): boolean;
begin
  if AText = '' then
    Result := not Closed
  else
    Result := SendBuffer(Pointer(AText)^, Length(AText));
end;

function TRALResponseStream.SendComment(const AText: StringRAL): boolean;
begin
  Result := Send(RALSSEComment(AText));
end;

function TRALResponseStream.SendEvent(const AData: StringRAL; const AEvent: StringRAL;
  const AId: StringRAL; ARetry: IntegerRAL): boolean;
begin
  Result := Send(RALSSEEvent(AData, AEvent, AId, ARetry));
end;

function TRALResponseStream.SendStream(AStream: TStream): boolean;
var
  vBuffer: array of Byte;
  vRead: IntegerRAL;
begin
  Result := not Closed;
  if (AStream = nil) or (not Result) then
    Exit;
  SetLength(vBuffer, DEFAULTBUFFERSTREAMSIZE);
  AStream.Position := 0;
  repeat
    vRead := AStream.Read(vBuffer[0], Length(vBuffer));
    if vRead > 0 then
      Result := SendBuffer(vBuffer[0], vRead);
  until (vRead <= 0) or (not Result);
end;

function TRALResponseStream.WaitClosed(ATimeout: Cardinal): boolean;
begin
  Result := FClosedEvent.WaitFor(ATimeout) = wrSignaled;
end;

end.
