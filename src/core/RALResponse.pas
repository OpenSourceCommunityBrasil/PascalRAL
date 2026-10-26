/// The response of a server or the answer a client receives.
unit RALResponse;

interface

uses
  Classes, SysUtils,
  RALTypes, RALParams, RALMIMETypes, RALCustomObjects, RALStream, RALConsts,
  RALCompress, RALTools, RALResponseStream;

type
  /// A response: status, headers, cookies and body.
  TRALResponse = class(TRALHTTPHeaderInfo)
  private
    /// Stream handed out by BodyStream while it is the body.
    FBodyStream: TStream;
    FCommitted: boolean;
    FContentEncoded: boolean;
    FDetached: boolean;
    FEngineContext: TObject;
    FErrorCode: IntegerRAL;
    FEventStream: boolean;
    { while True, a change to a committed response raises: on from the commit until
      the handler returns (TRALServer.ProcessCommands turns it off) }
    FGuarded: boolean;
    FResponseNotified: boolean;
    FStatusCode: IntegerRAL;
    FChannel: TRALResponseStream;
    /// Holds the channel by reference, so it outlives nobody's interest in it.
    FChannelRef: IRALResponseStream;
    FStreamCompress: TRALCompressType;
    FStreamRequest: TObject;
    FTransportError: TRALTransportError;

    /// Raises ERALResponseCommitted when the response is committed and guarded.
    procedure CheckNotCommitted;
    /// The commit (D1 of the plan): headers out, the body so far as the first piece.
    procedure Commit;
    procedure SetStatusCode(const AValue: IntegerRAL);
  protected
    function GetResponseStream: TStream;
    function GetResponseText: StringRAL;
    procedure SetResponseStream(const AValue: TStream); virtual; abstract;
    procedure SetResponseText(const AValue: StringRAL); virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    /// Adds AText as the body, of AContextType; returns the response.
    function AddBody(const AText: StringRAL; const AContextType: StringRAL = rctTEXTPLAIN): TRALResponse; reintroduce;
    { Adds a name=value cookie; returns the response.
      @deprecated Use AddCookie(ACookie: TRALCookie). }
    function AddCookie(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce; overload; deprecated 'use AddCookie(ACookie: TRALCookie) instead';
    /// Adds ACookie, with its attributes; returns the response.
    function AddCookie(const ACookie: TRALCookie): TRALResponse; reintroduce; overload;
    /// Adds a form field; returns the response.
    function AddField(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Adds the file AFileName to the body; returns the response.
    function AddFile(const AFileName: StringRAL): TRALResponse; reintroduce; overload;
    /// Adds AStream to the body as a file named AFileName; returns the response.
    function AddFile(AStream: TStream; const AFileName: StringRAL = ''): TRALResponse; reintroduce; overload;
    /// Adds a header; returns the response.
    function AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALResponse; reintroduce;
    /// Answers AStatusCode with the text AMessage, of AContentType.
    procedure Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    /// Answers AStatusCode with a copy of AStream; the caller keeps AStream.
    procedure Answer(AStatusCode: IntegerRAL; const AStream: TStream;
                     const AContentType: StringRAL = rctAPPLICATIONJSON); overload;
    { Answers AStatusCode with AStream; with AOwnsStream the response takes it,
      sends it as it is and frees it, otherwise it is copied. }
    procedure Answer(AStatusCode: IntegerRAL; AStream: TStream;
                     const AContentType: StringRAL; AOwnsStream: boolean); overload;
    /// Answers AStatusCode, with the server's page for it when there is one.
    procedure Answer(AStatusCode: IntegerRAL); overload;
    /// Answers with the file AFileName, inline unless DispositionInline is False.
    procedure Answer(const AFileName: StringRAL; const DispositionInline: boolean = true); overload;
    { Commits the response without a body piece yet: status and headers go out now
      (an SSE stream shows up at once). With no content type chosen, a stream begun
      here is text/event-stream }
    procedure BeginStream; overload;
    /// Commits with AStatusCode and AContentType.
    procedure BeginStream(AStatusCode: IntegerRAL; const AContentType: StringRAL); overload;
    { A stream to write the body into, owned by the response and sent as it is;
      asking again returns the same stream while it is still the body. }
    function BodyStream: TStream;
    /// Empties the response and restores its defaults.
    procedure Clear; override;
    { Keeps the stream open after the handler returns, for another thread to send
      on (an MCP GET, a notification feed); commits first. Raises on an engine that
      does not stream live (TRALServer.SupportsLiveStream) }
    function Detach: IRALResponseStream;
    /// Ends the body now (the last chunk); implicit when the handler returns, unless Detach.
    procedure EndStream;
    /// Adds the Set-Cookie values to ADest; ADateTime expires the plain cookies.
    procedure GetParamsCookies(ADest: TStringList; ADateTime: TDateTime);
    /// The Set-Cookie header lines of the cookies, each starting with AHeader.
    function GetParamsCookiesText(ADateTime: TDateTime; AHeader: StringRAL = 'Set-Cookie: ') : StringRAL;
    /// The body, encoded for the wire when AEncode, as a stream (see the descendants).
    function GetResponseEncStream(const AEncode: boolean = true): TStream; virtual; abstract;
    /// The body as text, encoded for the wire when AEncode.
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; virtual; abstract;
    { Sends a piece of the body to the client now. The first call commits the
      response: status and headers go out, with the body set so far as the first
      piece, and nothing of them can change afterwards (ERALResponseCommitted).
      False when the client is gone or the stream was closed - a disconnect is
      normal in a stream and never raises. On an engine that does not stream live
      the pieces are kept and go out together when the handler returns }
    function Send(const AText: StringRAL): boolean; overload;
    function Send(const ABuffer; ASize: IntegerRAL): boolean; overload;
    /// Sends AStream from its start; with AOwnsStream it is freed afterwards.
    function Send(AStream: TStream; AOwnsStream: boolean = True): boolean; overload;
    { Sends one Server-Sent Event (RALSSEEvent): AData as data: lines, with event,
      id and retry when given. With no content type chosen the stream is
      text/event-stream }
    function SendEvent(const AData: StringRAL; const AEvent: StringRAL = '';
      const AId: StringRAL = ''; ARetry: IntegerRAL = -1): boolean;
    { ENGINE API: True when the response went out as a live stream, so the engine
      sends nothing more and calls TRALServer.FinishStream }
    function StreamLive: boolean;
    function TakeWireStream: TStream; override;
    function TakeWireString: RawByteString; override;
    { ENGINE API: called by TRALServer.ProcessCommands when the handler returned;
      a committed response accepts the engine's own changes from here }
    procedure Unguard;

    /// True once Send, SendEvent, BeginStream or Detach committed the response.
    property Committed: boolean read FCommitted;
    /// The best answer the engine can give on whether the client is still there.
    function Connected: boolean;
    /// True once Detach kept the stream past the handler.
    property Detached: boolean read FDetached;
    { ENGINE API: what the engine needs to build its stream writer (its connection,
      its response object), set before ProcessCommands; owned by the engine }
    property EngineContext: TObject read FEngineContext write FEngineContext;
    /// True when the stream was begun as text/event-stream.
    property EventStream: boolean read FEventStream;
    /// The channel of a live stream; nil before the commit or when it is kept in memory.
    property Stream: TRALResponseStream read FChannel;
    /// The request this response answers, set by TRALServer.ProcessCommands.
    property StreamRequest: TObject read FStreamRequest write FStreamRequest;
    { How a streamed response is compressed: set by TRALCompressPlugin when
      CompressStreams is on (gzip and deflate only); ctNone sends it as it is }
    property StreamCompress: TRALCompressType read FStreamCompress write FStreamCompress;

    /// The body is already coded as ContentEncoding says, and is not compressed again.
    property ContentEncoded: boolean read FContentEncoded write FContentEncoded;
    /// The body, decoded: on a server a copy the caller frees, on a client the one kept.
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    /// The body, decoded, as text.
    property ResponseText: StringRAL read GetResponseText write SetResponseText;
  published
    /// Error of the transport on a client, 0 when an HTTP response arrived.
    property ErrorCode: IntegerRAL read FErrorCode write FErrorCode;
    /// HTTP status code.
    property StatusCode: IntegerRAL read FStatusCode write SetStatusCode;
    /// How the send attempt ended for the transport; rteNone when a response arrived.
    property TransportError: TRALTransportError read FTransportError
                                                write FTransportError;
  end;

  /// Response a server sends.
  TRALServerResponse = class(TRALResponse)
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  public
    /// The handler's body: encoded for the wire when AEncode, a plain copy otherwise.
    function GetResponseEncStream(const AEncode: boolean = true): TStream; override;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; override;
  end;

  /// Response a client receives.
  TRALClientResponse = class(TRALResponse)
  private
    /// Body assembled from the params when none went through the decoder.
    FStream: TStream;
  protected
    procedure SetResponseStream(const AValue: TStream); override;
    procedure SetResponseText(const AValue: StringRAL); override;
  public
    constructor Create(AOwner : TObject); override;
    destructor Destroy; override;

    procedure Clear; override;
    /// The body that arrived, decoded and owned by the response; AEncode is ignored.
    function GetResponseEncStream(const AEncode: boolean = true): TStream; override;
    function GetResponseEncText(const AEncode: boolean = true): StringRAL; override;
    procedure SetWireBody(AStream: TStream; AOwnership: TRALBodyOwnership); override;
  end;

implementation

uses
  RALServer, RALRequest, RALSSE;

{ TRALResponse }

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; const AMessage: StringRAL;
  const AContentType: StringRAL);
begin
  CheckNotCommitted;
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseText := AMessage;
end;

procedure TRALResponse.Answer(const AFileName: StringRAL;
  const DispositionInline: boolean);
begin
  CheckNotCommitted;
  AddFile(AFileName);
  ContentDispositionInline := DispositionInline;
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; const AStream: TStream;
  const AContentType: StringRAL);
begin
  CheckNotCommitted;
  StatusCode := AStatusCode;
  ContentType := AContentType;
  ResponseStream := AStream;
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL; AStream: TStream;
  const AContentType: StringRAL; AOwnsStream: boolean);
var
  vParam: TRALParam;
begin
  if not AOwnsStream then
  begin
    Answer(AStatusCode, AStream, AContentType);
    Exit;
  end;
  if FGuarded then
  begin
    AStream.Free;
    CheckNotCommitted;
  end;

  StatusCode := AStatusCode;
  ContentType := AContentType;
  Params.ClearParams(rpkBODY);
  if (AStream = nil) or (AStream.Size = 0) then
  begin
    AStream.Free;
    Exit;
  end;
  vParam := Params.NewParam;
  vParam.ParamName := 'ral_body';
  vParam.AdoptStream(AStream);
  vParam.ContentType := ContentType;
  vParam.Kind := rpkBODY;
end;

function TRALResponse.BodyStream: TStream;
var
  vParam: TRALParam;
begin
  // the same stream while it is still the body: several writes append
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) and (Params.Count(rpkBODY) = 1) then
  begin
    Result := FBodyStream;
    Exit;
  end;

  Params.ClearParams(rpkBODY);
  vParam := Params.NewParam;
  vParam.ParamName := 'ral_body';
  vParam.AdoptStream(TMemoryStream.Create);
  vParam.ContentType := ContentType;
  vParam.Kind := rpkBODY;
  FBodyStream := vParam.Content;
  Result := FBodyStream;
end;

function TRALResponse.TakeWireStream: TStream;
var
  vParam: TRALParam;
  vEncoding: StringRAL;
begin
  // BodyStream may have been asked before the handler set the content type
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) then
    vParam.ContentType := ContentType;
  FBodyStream := nil;
  { coded already: nothing compresses it again, and ContentEncoding goes out as
    written, even for a coding this program cannot produce }
  if FContentEncoded then
  begin
    vEncoding := ContentEncoding;
    ContentCompress := ctNone;
    Result := inherited TakeWireStream;
    ContentEncoding := vEncoding;
  end
  else
    Result := inherited TakeWireStream;
end;

function TRALResponse.TakeWireString: RawByteString;
var
  vParam: TRALParam;
  vEncoding: StringRAL;
begin
  vParam := Body;
  if (FBodyStream <> nil) and (vParam <> nil) and (not vParam.IsText) and
     (vParam.Content = FBodyStream) then
    vParam.ContentType := ContentType;
  FBodyStream := nil;
  if FContentEncoded then
  begin
    vEncoding := ContentEncoding;
    ContentCompress := ctNone;
    Result := inherited TakeWireString;
    ContentEncoding := vEncoding;
  end
  else
    Result := inherited TakeWireString;
end;

procedure TRALResponse.GetParamsCookies(ADest: TStringList; ADateTime: TDateTime);
var
  vInt: integer;
  vAttrs: StringRAL;
  vParam: TRALParam;
begin
  { A plain name=value cookie expires at ADateTime and carries Path=/, so the
    browser sends it to every route. }
  vAttrs := '; Expires=' + RALHTTPDate(RALDateTimeToGMT(ADateTime)) + '; Path=/';

  for vInt := 0 to Pred(Params.Count) do
  begin
    vParam := TRALParam(Params.Index[vInt]);
    if (vParam <> nil) and (vParam.Kind = rpkCOOKIE) then
    begin
      { A Set-Cookie param holds a whole cookie line and goes out as it is; every
        engine builds its cookies from here, so CR and LF are taken out here. }
      if RALSameName(vParam.ParamName, 'Set-Cookie') then
        ADest.Add(RALSafeHeaderText(vParam.AsString))
      else
        ADest.Add(RALSafeHeaderText(vParam.ParamName + '=' + vParam.AsString) + vAttrs);
    end;
  end;
end;

function TRALResponse.GetParamsCookiesText(ADateTime: TDateTime; AHeader: StringRAL): StringRAL;
var
  vDest: TStringList;
  vInt: integer;
begin
  Result := '';
  vDest := TStringList.Create;
  try
    GetParamsCookies(vDest, ADateTime);
    for vInt := 0 to Pred(vDest.Count) do
    begin
      if Result <> '' then
        Result := Result + HTTPLineBreak;
      Result := Result + AHeader + vDest.Strings[vInt];
    end;
  finally
    FreeAndNil(vDest);
  end;
end;

procedure TRALResponse.Clear;
begin
  inherited Clear;
  FBodyStream := nil;
  FCommitted := False;
  FDetached := False;
  FEventStream := False;
  FGuarded := False;
  FResponseNotified := False;
  FChannel := nil;
  FChannelRef := nil;
  FStatusCode := -1;
  FErrorCode := 0;
  FTransportError := rteNone;
  FContentEncoded := False;
end;

procedure TRALResponse.Answer(AStatusCode: IntegerRAL);
var
  vResp: StringRAL;
begin
  CheckNotCommitted;
  FStatusCode := AStatusCode;
  if AStatusCode >= HTTP_BadRequest then
    ContentType := rctTEXTHTML;

  if Parent.InheritsFrom(TRALServer) then begin
    vResp := TRALServer(Parent).ResponsePages.HTMLPage[AStatusCode];
    if vResp <> '' then
      ResponseText := vResp;
  end;
end;

function TRALResponse.AddHeader(const AName: StringRAL; const AValue: StringRAL): TRALResponse;
begin
  CheckNotCommitted;
  inherited AddHeader(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddField(const AName: StringRAL; const AValue: StringRAL
  ): TRALResponse;
begin
  inherited AddField(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddBody(const AText: StringRAL;
  const AContextType: StringRAL): TRALResponse;
begin
  inherited AddBody(AText, AContextType);
  Result := Self;
end;

function TRALResponse.AddCookie(const AName: StringRAL; const AValue: StringRAL
  ): TRALResponse;
begin
  CheckNotCommitted;
  inherited AddCookie(AName, AValue);
  Result := Self;
end;

function TRALResponse.AddCookie(const ACookie: TRALCookie): TRALResponse;
begin
  CheckNotCommitted;
  inherited AddCookie(ACookie);
  Result := Self;
end;

function TRALResponse.AddFile(const AFileName: StringRAL): TRALResponse;
begin
  inherited AddFile(AFileName);
  Result := Self;
end;

function TRALResponse.AddFile(AStream: TStream; const AFileName: StringRAL): TRALResponse;
begin
  inherited AddFile(AStream, AFileName);
  Result := Self;
end;

constructor TRALResponse.Create(AOwner: TObject);
begin
  inherited;
  ContentType := rctAPPLICATIONJSON;
end;

destructor TRALResponse.Destroy;
begin
  // the channel lives on while a Detach holder keeps it
  FChannel := nil;
  FChannelRef := nil;
  inherited;
end;

procedure TRALResponse.SetStatusCode(const AValue: IntegerRAL);
begin
  if AValue <> FStatusCode then
    CheckNotCommitted;
  FStatusCode := AValue;
end;

procedure TRALResponse.CheckNotCommitted;
begin
  if FGuarded then
    raise ERALResponseCommitted.Create(emResponseCommitted);
end;

procedure TRALResponse.Unguard;
begin
  FGuarded := False;
end;

function TRALResponse.StreamLive: boolean;
begin
  Result := FChannel <> nil;
end;

function TRALResponse.Connected: boolean;
begin
  if FChannel <> nil then
    Result := FChannel.Connected
  else
    Result := True;
end;

procedure TRALResponse.Commit;
var
  vServer: TRALServer;
  vWriter: TRALStreamWriter;
  vBody: TStream;
  vType: StringRAL;
  vCompress: TRALCompressType;
  vEncoder: TRALStreamCoder;
begin
  if FCommitted then
    Exit;
  if (Parent = nil) or not Parent.InheritsFrom(TRALServer) then
    raise ERALResponseCommitted.Create(emStreamNoServer);
  vServer := TRALServer(Parent);

  { the RAL cipher is IV + CBC + HMAC over the whole body: no piece of it is
    something a client can read, and sending in clear what was asked encrypted is
    not an option. Nothing went out yet, so the request still answers 500 }
  if ContentCripto <> crNone then
    raise ERALStreamCipher.Create(emStreamCipher);
  vServer.CheckStreamLimit;

  // the default JSON is nobody's choice: SendEvent and BeginStream make it SSE
  if FEventStream and ((ContentType = '') or
     (Pos(StringRAL(rctAPPLICATIONJSON), ContentType) = 1)) then
    ContentType := rctTEXTEVENTSTREAM;
  if Pos(StringRAL('event-stream'), ContentType) > 0 then
  begin
    FEventStream := True;
    // a proxy that waits for the whole body would hold every event back
    if Params.GetKind['Cache-Control', rpkHEADER] = nil then
      inherited AddHeader('Cache-Control', 'no-cache');
    if Params.GetKind['X-Accel-Buffering', rpkHEADER] = nil then
      inherited AddHeader('X-Accel-Buffering', 'no');
  end;

  // OnResponse fires once, before the first byte - here for a stream
  if not FResponseNotified then
  begin
    FResponseNotified := True;
    vServer.NotifyResponse(TRALRequest(FStreamRequest), Self);
  end;

  { a stream is compressed only when TRALCompressPlugin.CompressStreams asked
    for it, with a coding that can flush each piece, and a type worth it }
  vCompress := FStreamCompress;
  if not (vCompress in [ctDeflate, ctZLib, ctGZip]) then
    vCompress := ctNone;
  if (vCompress <> ctNone) and RALIsCompressedType(ContentType,
     Params.SkipCompressedTypes, Params.SkipCompressTypes) then
    vCompress := ctNone;
  ContentCompress := ctNone;
  FCommitted := True;
  FGuarded := True;

  vWriter := nil;
  if vServer.SupportsLiveStream then
    vWriter := vServer.CreateStreamWriter(TRALRequest(FStreamRequest), Self);
  { an engine that cannot stream live keeps the pieces: they go out at the end,
    after the body set before the first Send }
  if vWriter = nil then
  begin
    { the body goes out whole at the end, compressed by the usual way }
    ContentCompress := vCompress;
    if (FBodyStream = nil) or (Body = nil) or (Body.Content <> FBodyStream) then
    begin
      vBody := GetResponseEncStream(False);
      try
        BodyStream;
        if vBody <> nil then
          RALCopyStream(vBody, FBodyStream, vBody.Size);
      finally
        vBody.Free;
      end;
    end;
    Exit;
  end;

  { what the handler set before the first Send goes first; with no body the
    content type is the one chosen, not the empty one of an empty body }
  vType := ContentType;
  vBody := TakeWireStream;
  if vBody = nil then
    ContentType := vType;
  vEncoder := nil;
  if vCompress <> ctNone then
    vEncoder := RALCreateStreamEncoder(vCompress);
  // after TakeWireStream: the header only, the channel compresses
  if vEncoder <> nil then
    ContentEncoding := TRALCompress.CompressToString(vCompress);
  try
    FChannel := TRALResponseStream.Create(vWriter, FEventStream);
    FChannelRef := FChannel;
    FChannel.SetEncoder(vEncoder);
    vServer.RegisterStream(FChannel);
    vWriter.WriteHead(Self, FStatusCode);
    if vBody <> nil then
      FChannel.SendStream(vBody);
  finally
    vBody.Free;
  end;
end;

function TRALResponse.Send(const AText: StringRAL): boolean;
begin
  if AText = '' then
  begin
    Commit;
    Result := (FChannel = nil) or not FChannel.Closed;
    Exit;
  end;
  Result := Send(Pointer(AText)^, Length(AText));
end;

function TRALResponse.Send(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Commit;
  if FChannel <> nil then
    Result := FChannel.SendBuffer(ABuffer, ASize)
  else
  begin
    // kept: the body goes out whole when the handler returns
    if ASize > 0 then
      BodyStream.Write(ABuffer, ASize);
    Result := True;
  end;
end;

function TRALResponse.Send(AStream: TStream; AOwnsStream: boolean): boolean;
var
  vBuffer: array of Byte;
  vRead: IntegerRAL;
begin
  try
    Commit;
    Result := True;
    if AStream = nil then
      Exit;
    if FChannel <> nil then
      Result := FChannel.SendStream(AStream)
    else
    begin
      SetLength(vBuffer, DEFAULTBUFFERSTREAMSIZE);
      AStream.Position := 0;
      repeat
        vRead := AStream.Read(vBuffer[0], Length(vBuffer));
        if vRead > 0 then
          BodyStream.Write(vBuffer[0], vRead);
      until vRead <= 0;
    end;
  finally
    if AOwnsStream then
      AStream.Free;
  end;
end;

function TRALResponse.SendEvent(const AData: StringRAL; const AEvent: StringRAL;
  const AId: StringRAL; ARetry: IntegerRAL): boolean;
begin
  if not FCommitted then
    FEventStream := True;
  Result := Send(RALSSEEvent(AData, AEvent, AId, ARetry));
end;

procedure TRALResponse.BeginStream;
begin
  if not FCommitted then
    FEventStream := True;
  Commit;
end;

procedure TRALResponse.BeginStream(AStatusCode: IntegerRAL; const AContentType: StringRAL);
begin
  if not FCommitted then
  begin
    StatusCode := AStatusCode;
    ContentType := AContentType;
  end;
  Commit;
end;

procedure TRALResponse.EndStream;
begin
  if FChannel <> nil then
    FChannel.Close;
end;

function TRALResponse.Detach: IRALResponseStream;
begin
  if (Parent <> nil) and Parent.InheritsFrom(TRALServer) and
     not TRALServer(Parent).SupportsLiveStream then
    raise ERALResponseCommitted.Create(emStreamNoLive);
  Commit;
  if FChannel = nil then
    raise ERALResponseCommitted.Create(emStreamNoLive);
  FDetached := True;
  Result := FChannelRef;
end;

function TRALResponse.GetResponseStream: TStream;
begin
  // decoded on both sides: on a server, what the handler answered
  Result := GetResponseEncStream(False);
end;

function TRALResponse.GetResponseText: StringRAL;
begin
  // the body as text, never what goes on the wire (that is GetResponseEncText)
  Result := GetResponseEncText(False);
end;

{ TRALServerResponse }

function TRALServerResponse.GetResponseEncStream(const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vSource: TStream;
  vBody: TRALBodyStream;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
  vKey: StringRAL;
begin
  Result := nil;
  vContentType := '';
  vContentDisposition := '';
  if not AEncode then
  begin
    // what the handler answered, as the caller's own copy
    vSource := Params.PlainBody(vContentType, vContentDisposition);
    if vSource = nil then
      Exit;
    try
      vBody := TRALBodyStream.Create(vSource.Size);
      try
        vSource.Position := 0;
        RALCopyStream(vSource, vBody, vSource.Size);
        Result := vBody.Detach;
      finally
        vBody.Free;
      end;
    finally
      vSource.Free;
    end;
    Result.Position := 0;
    Exit;
  end;

  // encoded, for an engine without TakeWireStream; the params are restored after
  vCompress := Params.CompressType;
  vCripto := Params.CriptoOptions.CriptType;
  vKey := Params.CriptoOptions.Key;
  try
    Params.CriptoOptions.CriptType := ContentCripto;
    Params.CriptoOptions.Key := CriptoKey;
    if FContentEncoded then
      Params.CompressType := ctNone // coded already - see ContentEncoded
    else
      Params.CompressType := ContentCompress;
    Params.ContentDispositionInline := ContentDispositionInline;

    Result := Params.EncodeBody(vContentType, vContentDisposition);
    ContentType := vContentType;
    ContentDisposition := vContentDisposition;
    // what was done, not what was asked; a body coded already keeps its ContentEncoding
    if Result = nil then
      ContentCompress := ctNone
    else if not FContentEncoded then
      ContentCompress := Params.CompressType;
  finally
    Params.CompressType := vCompress;
    Params.CriptoOptions.CriptType := vCripto;
    Params.CriptoOptions.Key := vKey;
  end;
end;

function TRALServerResponse.GetResponseEncText(
  const AEncode: boolean): StringRAL;
var
  vStream: TStream;
  vContentType, vContentDisposition: StringRAL;
begin
  if not AEncode then
  begin
    { a text answer is the handler's own string, not a copy of it }
    vContentType := '';
    vContentDisposition := '';
    vStream := Params.PlainBody(vContentType, vContentDisposition);
    try
      Result := RALStreamText(vStream);
    finally
      vStream.Free;
    end;
    Exit;
  end;

  vStream := GetResponseEncStream(AEncode);
  try
    Result := StreamToString(vStream);
  finally
    FreeAndNil(vStream);
  end;
end;

procedure TRALServerResponse.SetResponseStream(const AValue: TStream);
var
  vParam: TRALParam;
begin
  CheckNotCommitted;
  Params.ClearParams(rpkBODY);
  if Assigned(Avalue) and (AValue.Size > 0) then
  begin
    vParam := Params.AddValue(AValue, rpkBODY);
    vParam.ContentType := ContentType;
  end;
end;

procedure TRALServerResponse.SetResponseText(const AValue: StringRAL);
var
  vParam: TRALParam;
begin
  CheckNotCommitted;
  Params.ClearParams(rpkBODY);
  if AValue <> '' then
  begin
    vParam := Params.AddValue(AValue);
    vParam.ContentType := ContentType;
    vParam.Kind := rpkBODY;
  end;
end;

{ TRALClientResponse }

constructor TRALClientResponse.Create(AOwner : TObject);
begin
  inherited;
  FStream := nil;
end;

destructor TRALClientResponse.Destroy;
begin
  if FStream <> nil then
    FreeAndNil(FStream);
  inherited;
end;

procedure TRALClientResponse.Clear;
begin
  // a response is reused across the attempts of one call
  FreeAndNil(FStream);
  inherited;
end;

procedure TRALClientResponse.SetWireBody(AStream: TStream;
  AOwnership: TRALBodyOwnership);
begin
  FreeAndNil(FStream);
  inherited;
end;

function TRALClientResponse.GetResponseEncStream(
  const AEncode: boolean): TStream;
var
  vContentType, vContentDisposition: StringRAL;
  vCompress: TRALCompressType;
  vCripto: TRALCriptoType;
begin
  // the body as it arrived, decoded once, with no copy
  Result := Params.Decoded;
  if Result <> nil then
  begin
    Result.Position := 0;
    Exit;
  end;

  { no body went through the decoder (params filled by hand): assembled from
    the params, once, and only when asked }
  if FStream = nil then
  begin
    vCompress := Params.CompressType;
    vCripto := Params.CriptoOptions.CriptType;
    Params.CompressType := ctNone;
    Params.CriptoOptions.CriptType := crNone;
    try
      FStream := Params.EncodeBody(vContentType, vContentDisposition);
    finally
      Params.CompressType := vCompress;
      Params.CriptoOptions.CriptType := vCripto;
    end;
  end;
  Result := FStream;
  if Result <> nil then
    Result.Position := 0;
end;

function TRALClientResponse.GetResponseEncText(
  const AEncode: boolean): StringRAL;
begin
  // a body delivered as a string comes back as that string
  Result := RALStreamText(GetResponseEncStream(AEncode));
end;

procedure TRALClientResponse.SetResponseStream(const AValue: TStream);
begin
  // decoded with what Params says; AValue is copied (SetWireBody adopts)
  FreeAndNil(FStream);
  if Assigned(AValue) and (AValue.size > 0) then
    Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

procedure TRALClientResponse.SetResponseText(const AValue: StringRAL);
begin
  FreeAndNil(FStream);
  Params.DecodeBody(AValue, ContentType, ContentDisposition);
end;

end.
