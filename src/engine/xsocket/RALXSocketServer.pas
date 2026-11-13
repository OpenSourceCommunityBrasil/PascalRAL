{ Server engine on Delphi-Cross-Socket (github.com/winddriver/Delphi-Cross-Socket):
  IOCP on Windows, epoll on Linux, TLS through the library's OpenSSL. Delphi only.
  The library parses the request (headers, cookies, a form body into fields, a
  gzip or deflate body decoded) and sends the answer; this unit turns that into
  the RAL pipeline and back. A live stream is pulled by the library from a
  queue the handler writes into. }
unit RALXSocketServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, DateUtils,
  RALTypes, RALConsts, RALTools, RALServer, RALRequest, RALResponse, RALParams,
  RALCustomObjects, RALResponseStream, RALStream, RALExceptions,
  Net.CrossSocket.Base, Net.CrossHttpServer;

const
  /// Name of the engine, in TRALServer.Engine and TRALClient.EngineType.
  ENGINEXSOCKET = 'XSocket';

type
  TRALXSocketServer = class;

  { TLS of the Cross-Socket server: the certificate and its key as PEM files, or
    as PEM text (what RALSelfSigned hands over). }
  TRALXSocketSSL = class(TRALSSL)
  private
    FCertFile: StringRAL;
    FCertPEM: StringRAL;
    FKeyFile: StringRAL;
    FKeyPEM: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    /// Certificate as PEM text (set by SetTLSCertificate, RALSelfSigned).
    property CertPEM: StringRAL read FCertPEM write FCertPEM;
    /// Key as PEM text.
    property KeyPEM: StringRAL read FKeyPEM write FKeyPEM;
  published
    /// Certificate (PEM, with its chain).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Private key of CertFile (PEM, not encrypted: the library takes no password).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
  end;

  { What the engine keeps of a request while it runs, reachable from the response
    (TRALResponse.EngineContext): the library's response, for the live writer. }
  TRALXSocketContext = class
  private
    /// The response of the library.
    FResponse: ICrossHttpResponse;
  public
    /// A context of the library's AResponse.
    constructor Create(const AResponse: ICrossHttpResponse);

    /// The library's response of the request.
    property Response: ICrossHttpResponse read FResponse;
  end;

  { A request waiting for a worker: the library's request and response, whose
    reference counts keep them alive. }
  TRALXSocketWork = class
  private
    /// The request of the library.
    FRequest: ICrossHttpRequest;
    /// The response of the library.
    FResponse: ICrossHttpResponse;
  public
    /// A work item of ARequest and AResponse.
    constructor Create(const ARequest: ICrossHttpRequest;
                       const AResponse: ICrossHttpResponse);
  end;

  /// A worker thread that runs the requests of the queue.
  TRALXSocketWorker = class(TThread)
  private
    /// The server that owns it.
    FServer: TRALXSocketServer;
  protected
    procedure Execute; override;
  public
    /// A worker of AServer, already running.
    constructor Create(AServer: TRALXSocketServer);
  end;

  { What the handler thread and the library share while a live answer goes out:
    the pieces written and not yet pulled, and the end. The library pulls the next
    piece when the previous one is on the wire, so Pull waits for the handler. }
  TRALXSocketPipe = class(TInterfacedObject)
  private
    /// The answer was cut: the connection closes.
    FAborted: boolean;
    /// The piece the library is sending: it must stay until the next pull.
    FCurrent: TBytes;
    /// Set while less than the high water mark is queued.
    FDrained: TEvent;
    /// The handler wrote the last piece.
    FEnded: boolean;
    /// Set when a piece or the end arrived.
    FEvent: TEvent;
    /// Guards the queue and the flags.
    FLock: TCriticalSection;
    /// Bytes queued and not yet pulled.
    FPending: Int64RAL;
    /// The pieces written and not yet pulled.
    FQueue: TList<TBytes>;
    /// The response of the library.
    FResponse: ICrossHttpResponse;
    /// The thread that hands the pipe to the library was started.
    FStarted: boolean;

    /// True while the connection is open.
    function ConnectionOpen: boolean;
  public
    /// A pipe that answers through AResponse.
    constructor Create(const AResponse: ICrossHttpResponse);
    destructor Destroy; override;

    /// True while the answer can still be written.
    function Alive: boolean;
    /// True once the answer ended, whole or cut.
    function Ended: boolean;
    /// Ends the answer; AAborted closes the connection without the last chunk.
    procedure Finish(AAborted: boolean);
    { Called by the library for the next piece: waits for one, False at the end.
      The piece is valid until the next call. }
    function Pull(const AData: PPointer; const ACount: PNativeInt): boolean;
    { Queues a copy of ABuffer; waits while more than the high water mark is
      queued, at most ATimeout milliseconds (0 is no limit). }
    function Push(const ABuffer; ASize: IntegerRAL; ATimeout: IntegerRAL): boolean;
    { Starts the thread that hands the pipe to the library; the head must be
      filled in the response. }
    procedure Start;
  end;

  /// Writes a live answer of the pipe; the library chunks it.
  TRALXSocketStreamWriter = class(TRALStreamWriter)
  private
    /// The pipe of this answer.
    FPipe: TRALXSocketPipe;
    /// Keeps the pipe alive while the writer lives.
    FPipeRef: IInterface;
    /// The response of the library.
    FResponse: ICrossHttpResponse;
    /// The server that owns it.
    FServer: TRALXSocketServer;
  public
    /// A writer of the library's AResponse.
    constructor Create(AServer: TRALXSocketServer; const AResponse: ICrossHttpResponse);
    destructor Destroy; override;

    function Alive: boolean; override;
    procedure WriteEnd(AAborted: boolean); override;
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

  { RAL server on Delphi-Cross-Socket. The requests run on PoolCount worker
    threads, or on the library's IO threads with ExecuteInIOThread. }
  TRALXSocketServer = class(TRALServer)
  private
    /// The library's server.
    FHttp: ICrossHttpServer;
    FExecuteInIOThread: boolean;
    FIoThreads: IntegerRAL;
    /// Live writers now.
    FLiveStreams: IntegerRAL;
    FPoolCount: IntegerRAL;
    /// The pieces written and not yet pulled.
    FQueue: TThreadedQueue<TRALXSocketWork>;
    /// The worker threads.
    FWorkers: TList;

    /// A connection was accepted: TCP_NODELAY.
    procedure Connected(const Sender: TObject; const AConnection: ICrossConnection);
    function GetSSL: TRALXSocketSSL;
    /// OnRequest of the library: runs the request here or queues it.
    procedure HttpRequest(const Sender: TObject; const AConnection: ICrossHttpConnection;
                          const ARequest: ICrossHttpRequest;
                          const AResponse: ICrossHttpResponse; var AHandled: boolean);
    procedure SetSSL(const AValue: TRALXSocketSSL);
    /// Starts the library's server and the workers.
    procedure StartServer;
    /// Stops the workers and the library's server.
    procedure StopServer;
  protected
    /// Answers 500 for an exception outside the pipeline, through the server's rules.
    procedure AnswerFailure(const AResponse: ICrossHttpResponse; AError: Exception);
    function CreateRALSSL: TRALSSL; override;
    function IPv6IsImplemented: boolean; override;
    /// Takes the next queued request for a worker; nil when the server stops.
    function NextRequest: TRALXSocketWork;
    /// Runs one request through the RAL pipeline and answers it.
    procedure RunRequest(const ARequest: ICrossHttpRequest;
                         const AResponse: ICrossHttpResponse);
    procedure SetActive(const AValue: boolean); override;
    /// A live writer ended: its slot of the IO threads is free again.
    procedure StreamEnded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; override;
    function GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean; override;
    function SetTLSCertificate(const ACertificate: TRALTLSCertificate): TRALTLSApplyResult; override;
    class function SupportsLiveStream: boolean; override;
    function TLSProvisioning: TRALTLSProvisioning; override;
  published
    { IO threads of the library; 0 = twice the processors plus one. A live
      stream keeps one of them waiting, so at most IoThreads - 1 streams are live
      at once; the others are answered whole. }
    property IoThreads: IntegerRAL read FIoThreads write FIoThreads default 0;
    { Runs the requests on the IO threads instead of the workers: faster for short
      handlers (about 20% on a 2-processor machine), but a handler that blocks
      (a database, a call to another server) or streams holds an IO thread and
      every connection on it waits. }
    property ExecuteInIOThread: boolean read FExecuteInIOThread write FExecuteInIOThread default False;
    /// Worker threads that run the requests; 0 = four per processor. Unused with ExecuteInIOThread.
    property PoolCount: IntegerRAL read FPoolCount write FPoolCount default 0;
    /// TLS: the certificate and key files, or PEM text.
    property SSL: TRALXSocketSSL read GetSSL write SetSSL;
  end;

implementation

uses
  Net.CrossHttpParams, Net.CrossHttpUtils, Net.SocketAPI;

const
  /// Bytes a live writer may queue before it waits for the library.
  cHighWater = 1024 * 1024;
  /// Headers that belong to one connection and are never copied from RAL.
  cHopByHop: array[0..8] of string = ('connection', 'keep-alive', 'proxy-authenticate',
    'proxy-authorization', 'te', 'trailer', 'transfer-encoding', 'upgrade',
    'content-length');

/// True for a header of the connection, never copied from RAL.
function IsHopByHop(const AName: StringRAL): boolean;
var
  vInt: IntegerRAL;
begin
  for vInt := Low(cHopByHop) to High(cHopByHop) do
    if RALSameName(StringRAL(cHopByHop[vInt]), AName) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

{ What RAL says about the head of an answer, in the library's response: status,
  the headers and one Set-Cookie line per cookie. }
procedure FillLibraryHead(const ALibrary: ICrossHttpResponse; AServer: TRALServer;
  AResponse: TRALResponse; AStatusCode: IntegerRAL);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vCookies: TStringList;
  vHasCookies: boolean;
begin
  ALibrary.StatusCode := AStatusCode;
  if AResponse.ContentType <> '' then
    ALibrary.ContentType := string(AResponse.ContentType);
  if AResponse.ContentEncoding <> '' then
    ALibrary.Header['Content-Encoding'] := string(AResponse.ContentEncoding);
  if AResponse.ContentDisposition <> '' then
    ALibrary.Header['Content-Disposition'] := string(AResponse.ContentDisposition);
  if AResponse.ContentEncription <> '' then
    ALibrary.Header['Content-Encription'] := string(AResponse.ContentEncription);
  vHasCookies := False;
  for vInt := 0 to AResponse.Params.Count - 1 do
  begin
    vParam := AResponse.Params.Index[vInt];
    if vParam.Kind = rpkCOOKIE then
      vHasCookies := True;
    if (vParam.Kind = rpkHEADER) and (vParam.ParamName <> '') and
       not IsHopByHop(vParam.ParamName) and
       not RALSameName(vParam.ParamName, 'content-type') and
       not RALSameName(vParam.ParamName, 'content-encoding') and
       not RALSameName(vParam.ParamName, 'content-disposition') and
       not RALSameName(vParam.ParamName, 'content-encription') then
      ALibrary.Header.Add(string(vParam.ParamName),
        string(RALSafeHeaderText(vParam.AsString)), True);
  end;
  if vHasCookies then
  begin
    vCookies := TStringList.Create;
    try
      AResponse.GetParamsCookies(vCookies, IncMinute(Now, AServer.CookieLife));
      for vInt := 0 to vCookies.Count - 1 do
        ALibrary.Header.Add('Set-Cookie', vCookies[vInt], True);
    finally
      vCookies.Free;
    end;
  end;
end;

{ The head of an answer to HEAD, written on the connection: the library always
  sends the body it is given, and a HEAD must carry the length of the GET with
  none. The connection is closed after it. }
function HeadBytes(const ALibrary: ICrossHttpResponse; ALength: Int64RAL): TBytes;
var
  vText: string;
  vInt: IntegerRAL;
begin
  if ALibrary.ContentType = '' then
    ALibrary.ContentType := 'application/octet-stream';
  vText := 'HTTP/1.1 ' + IntToStr(ALibrary.StatusCode) + ' ' +
    TCrossHttpUtils.GetHttpStatusText(ALibrary.StatusCode) + #13#10;
  for vInt := 0 to ALibrary.Header.Count - 1 do
    if not IsHopByHop(StringRAL(ALibrary.Header.Items[vInt].Name)) then
      vText := vText + ALibrary.Header.Items[vInt].Name + ': ' +
        ALibrary.Header.Items[vInt].Value + #13#10;
  vText := vText + 'Content-Length: ' + IntToStr(ALength) + #13#10 +
    'Connection: close'#13#10#13#10;
  Result := TEncoding.ANSI.GetBytes(vText);
end;

{ TRALXSocketSSL }

procedure TRALXSocketSSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALXSocketSSL then
  begin
    TRALXSocketSSL(Dest).FCertFile := FCertFile;
    TRALXSocketSSL(Dest).FCertPEM := FCertPEM;
    TRALXSocketSSL(Dest).FKeyFile := FKeyFile;
    TRALXSocketSSL(Dest).FKeyPEM := FKeyPEM;
  end;
end;

{ TRALXSocketContext }

constructor TRALXSocketContext.Create(const AResponse: ICrossHttpResponse);
begin
  inherited Create;
  FResponse := AResponse;
end;

{ TRALXSocketWork }

constructor TRALXSocketWork.Create(const ARequest: ICrossHttpRequest;
  const AResponse: ICrossHttpResponse);
begin
  inherited Create;
  FRequest := ARequest;
  FResponse := AResponse;
end;

{ TRALXSocketWorker }

constructor TRALXSocketWorker.Create(AServer: TRALXSocketServer);
begin
  FServer := AServer;
  inherited Create(False);
end;

procedure TRALXSocketWorker.Execute;
var
  vWork: TRALXSocketWork;
begin
  while not Terminated do
  begin
    vWork := FServer.NextRequest;
    if vWork = nil then
      Continue;
    try
      FServer.RunRequest(vWork.FRequest, vWork.FResponse);
    finally
      vWork.Free;
    end;
  end;
end;

{ TRALXSocketPipe }

constructor TRALXSocketPipe.Create(const AResponse: ICrossHttpResponse);
begin
  inherited Create;
  FResponse := AResponse;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FDrained := TEvent.Create(nil, True, True, '');
  FQueue := TList<TBytes>.Create;
end;

destructor TRALXSocketPipe.Destroy;
begin
  FQueue.Free;
  FDrained.Free;
  FEvent.Free;
  FLock.Free;
  inherited Destroy;
end;

function TRALXSocketPipe.Alive: boolean;
begin
  FLock.Acquire;
  try
    Result := not (FEnded or FAborted);
  finally
    FLock.Release;
  end;
  Result := Result and ConnectionOpen;
end;

function TRALXSocketPipe.Ended: boolean;
begin
  FLock.Acquire;
  try
    Result := FEnded;
  finally
    FLock.Release;
  end;
end;

function TRALXSocketPipe.ConnectionOpen: boolean;
begin
  Result := (FResponse <> nil) and (FResponse.Connection <> nil) and
    not FResponse.Connection.IsClosed;
end;

procedure TRALXSocketPipe.Finish(AAborted: boolean);
begin
  FLock.Acquire;
  try
    FEnded := True;
    if AAborted then
      FAborted := True;
  finally
    FLock.Release;
  end;
  if AAborted and (FResponse <> nil) and (FResponse.Connection <> nil) then
    FResponse.Connection.Disconnect;
  FEvent.SetEvent;
  FDrained.SetEvent;
end;

function TRALXSocketPipe.Pull(const AData: PPointer; const ACount: PNativeInt): boolean;
begin
  Result := False;
  while True do
  begin
    FLock.Acquire;
    try
      if FAborted then
        Exit;
      if FQueue.Count > 0 then
      begin
        FCurrent := FQueue[0];
        FQueue.Delete(0);
        Dec(FPending, Length(FCurrent));
        if FPending < cHighWater then
          FDrained.SetEvent;
        AData^ := @FCurrent[0];
        ACount^ := Length(FCurrent);
        Result := True;
        Exit;
      end;
      if FEnded then
        Exit;
    finally
      FLock.Release;
    end;
    // the peer left while nothing was written: the answer is over
    if not ConnectionOpen then
    begin
      Finish(True);
      Exit;
    end;
    FEvent.WaitFor(200);
  end;
end;

function TRALXSocketPipe.Push(const ABuffer; ASize: IntegerRAL;
  ATimeout: IntegerRAL): boolean;
var
  vPiece: TBytes;
  vWait: Cardinal;
begin
  Result := False;
  if ASize <= 0 then
  begin
    Result := Alive;
    Exit;
  end;
  SetLength(vPiece, ASize);
  Move(ABuffer, vPiece[0], ASize);
  FLock.Acquire;
  try
    if FEnded or FAborted then
      Exit;
    FQueue.Add(vPiece);
    Inc(FPending, ASize);
    if FPending >= cHighWater then
      FDrained.ResetEvent;
  finally
    FLock.Release;
  end;
  FEvent.SetEvent;
  // back pressure: the library has not taken what was queued
  if ATimeout > 0 then
    vWait := ATimeout
  else
    vWait := INFINITE;
  if FDrained.WaitFor(vWait) <> wrSignaled then
    Exit;
  Result := Alive;
end;

procedure TRALXSocketPipe.Start;
var
  vKeep: IInterface;
begin
  if FStarted then
    Exit;
  FStarted := True;
  { the source below is called by the library for as long as the answer lasts,
    long after this thread ended, and after the writer was freed: it holds the pipe
    through vKeep, which nothing may clear }
  vKeep := Self;
  { the library calls Pull for the first piece in the thread that sends, and may
    go on calling it there when the sends complete at once: that thread cannot be
    the handler's, which has to write the pieces }
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FResponse.SendNoCompress(
          function(const AData: PPointer; const ACount: PNativeInt): boolean
          begin
            Result := TRALXSocketPipe(vKeep).Pull(AData, ACount);
          end);
      except
        Finish(True);
      end;
    end).Start;
end;

{ TRALXSocketStreamWriter }

constructor TRALXSocketStreamWriter.Create(AServer: TRALXSocketServer;
  const AResponse: ICrossHttpResponse);
begin
  inherited Create;
  FServer := AServer;
  FResponse := AResponse;
  FPipe := TRALXSocketPipe.Create(AResponse);
  FPipeRef := FPipe;
  FramesItself := True;
  Chunked := False;
end;

destructor TRALXSocketStreamWriter.Destroy;
begin
  // a writer freed before WriteEnd (a failed commit) must not leave the pull waiting
  if not FPipe.Ended then
    FPipe.Finish(True);
  FPipeRef := nil;
  FServer.StreamEnded;
  inherited Destroy;
end;

function TRALXSocketStreamWriter.Alive: boolean;
begin
  Result := FPipe.Alive;
end;

procedure TRALXSocketStreamWriter.WriteEnd(AAborted: boolean);
begin
  // nothing was ever written: the head still has to go out
  FPipe.Start;
  FPipe.Finish(AAborted);
end;

procedure TRALXSocketStreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo;
  AStatusCode: IntegerRAL);
begin
  FillLibraryHead(FResponse, FServer, TRALResponse(AResponse), AStatusCode);
  FPipe.Start;
end;

function TRALXSocketStreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := FPipe.Push(ABuffer, ASize, SendTimeout);
end;

{ TRALXSocketServer }

constructor TRALXSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWorkers := TList.Create;
  Port := 8000;
  SetEngine(ENGINEXSOCKET);
end;

destructor TRALXSocketServer.Destroy;
begin
  Active := False;
  FWorkers.Free;
  inherited Destroy;
end;

procedure TRALXSocketServer.AnswerFailure(const AResponse: ICrossHttpResponse;
  AError: Exception);
var
  vText: TBytes;
begin
  try
    if not AResponse.Sent then
    begin
      AResponse.Header.Clear;
      AResponse.StatusCode := HTTP_InternalError;
      AResponse.ContentType := 'text/plain';
      vText := TEncoding.UTF8.GetBytes(string(ErrorText(AError)));
      AResponse.SendNoCompress(vText);
    end
    else if AResponse.Connection <> nil then
      // the head went out already: cut, as HTTP says a truncated body
      AResponse.Connection.Disconnect;
  except
    // the connection is gone
  end;
  if Assigned(OnServerError) then
    OnServerError(AError)
  else if RaiseError then
    raise AError;
end;

procedure TRALXSocketServer.Connected(const Sender: TObject;
  const AConnection: ICrossConnection);
begin
  // two sends per answer (head and body) would otherwise wait for the ACK
  TSocketAPI.SetTcpNoDelay(AConnection.Socket, True);
end;

function TRALXSocketServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALXSocketSSL.Create;
end;

function TRALXSocketServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
var
  vContext: TRALXSocketContext;
begin
  Result := nil;
  if not (AResponse.EngineContext is TRALXSocketContext) then
    Exit;
  // a HEAD has no pieces and HTTP/1.0 cannot be chunked: answered whole
  if (ARequest.Method = amHEAD) or RALSameName(ARequest.Protocol, 'HTTP/1.0') then
    Exit;
  // each live stream keeps one IO thread waiting for its next piece
  if (FHttp = nil) or (RALAtomicInc(FLiveStreams) > FHttp.IoThreads - 1) then
  begin
    if FHttp <> nil then
      RALAtomicDec(FLiveStreams);
    Exit;
  end;
  vContext := TRALXSocketContext(AResponse.EngineContext);
  Result := TRALXSocketStreamWriter.Create(Self, vContext.Response);
end;

function TRALXSocketServer.GetSSL: TRALXSocketSSL;
begin
  Result := TRALXSocketSSL(GetDefaultSSL);
end;

function TRALXSocketServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  ACertificate.CertificatePEM := SSL.CertPEM;
  ACertificate.PrivateKeyPEM := SSL.KeyPEM;
  Result := (SSL.CertFile <> '') or (SSL.CertPEM <> '');
end;

procedure TRALXSocketServer.HttpRequest(const Sender: TObject;
  const AConnection: ICrossHttpConnection; const ARequest: ICrossHttpRequest;
  const AResponse: ICrossHttpResponse; var AHandled: boolean);
var
  vWork: TRALXSocketWork;
begin
  AHandled := True;
  if FQueue <> nil then
  begin
    vWork := TRALXSocketWork.Create(ARequest, AResponse);
    if FQueue.PushItem(vWork) <> wrSignaled then
      vWork.Free;
  end
  else
    RunRequest(ARequest, AResponse);
end;

function TRALXSocketServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

function TRALXSocketServer.NextRequest: TRALXSocketWork;
begin
  Result := nil;
  if (FQueue <> nil) and (FQueue.PopItem(Result) <> wrSignaled) then
    Result := nil;
end;

procedure TRALXSocketServer.RunRequest(const ARequest: ICrossHttpRequest;
  const AResponse: ICrossHttpResponse);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vContext: TRALXSocketContext;
  vBody: TMemoryStream;
  vWire: TStream;
  vHeader: THttpHeader;
  vInt: IntegerRAL;
  vName, vValue, vCookies, vType: StringRAL;
  vText: TBytes;
  vLength: Int64RAL;
  vEncoding: string;
begin
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  vContext := TRALXSocketContext.Create(AResponse);
  vBody := nil;
  try
    try
      vRequest.Query := StringRAL(ARequest.RawPathAndParams);
      vRequest.Method := HTTPMethodToRALMethod(StringRAL(ARequest.Method));
      vRequest.Protocol := StringRAL(ARequest.Version);
      vHeader := ARequest.Header;
      vRequest.Host := StringRAL(vHeader['Host']);
      if ARequest.Connection.Ssl then
        vRequest.HttpVersion := 'HTTPS'
      else
        vRequest.HttpVersion := 'HTTP';
      vRequest.ClientInfo.IP := StringRAL(ARequest.Connection.PeerAddr);
      vRequest.ClientInfo.Port := ARequest.Connection.PeerPort;
      vRequest.ClientInfo.ConnectionID := Int64(ARequest.Connection.UID);
      // the library decoded a gzip or deflate body: the header says what it was
      vEncoding := ARequest.ContentEncoding;
      vCookies := '';
      for vInt := 0 to vHeader.Count - 1 do
      begin
        vName := StringRAL(vHeader.Items[vInt].Name);
        vValue := StringRAL(vHeader.Items[vInt].Value);
        if RALSameName(vName, 'content-type') then
          vRequest.ContentType := vValue
        else if RALSameName(vName, 'content-encoding') then
        begin
          if not (SameText(vEncoding, 'gzip') or SameText(vEncoding, 'deflate')) then
            vRequest.ContentEncoding := vValue;
        end
        else if RALSameName(vName, 'accept-encoding') then
          vRequest.AcceptEncoding := vValue
        else if RALSameName(vName, 'content-encription') then
          vRequest.ContentEncription := vValue
        else if RALSameName(vName, 'accept-encription') then
          vRequest.AcceptEncription := vValue
        else if RALSameName(vName, 'user-agent') then
          vRequest.ClientInfo.UserAgent := vValue
        else if RALSameName(vName, 'cookie') then
        begin
          if vCookies <> '' then
            vCookies := vCookies + '; ';
          vCookies := vCookies + vValue;
        end;
        vRequest.Params.AddParam(vName, vValue, rpkHEADER);
      end;
      if vCookies <> '' then
        vRequest.AddCookies(vCookies);

      // the body the way it came: the library turned a form into fields
      case ARequest.BodyType of
        btUrlEncoded:
          if ARequest.Body is THttpUrlParams then
          begin
            vBody := TMemoryStream.Create;
            vText := TEncoding.UTF8.GetBytes(THttpUrlParams(ARequest.Body).Encode);
            if Length(vText) > 0 then
              vBody.WriteBuffer(vText[0], Length(vText));
          end;
        btMultiPart:
          if ARequest.Body is THttpMultiPartFormData then
          begin
            vBody := TMemoryStream.Create;
            vWire := THttpMultiPartFormStream.Create(THttpMultiPartFormData(ARequest.Body));
            try
              vBody.CopyFrom(vWire, 0);
            finally
              vWire.Free;
            end;
          end;
      end;
      if (ARequest.BodyType = btBinary) and (ARequest.Body is TStream) then
        vLength := TStream(ARequest.Body).Size
      else if vBody <> nil then
        vLength := vBody.Size
      else
        vLength := 0;
      vRequest.ContentSize := vLength;

      DecodeAuth(vRequest);
      ValidateRequest(vRequest, vResponse);
      if vResponse.StatusCode < HTTP_BadRequest then
      begin
        if vBody <> nil then
        begin
          vBody.Position := 0;
          vRequest.SetWireBody(vBody, boOwned);
          vBody := nil;
        end
        else if (ARequest.BodyType = btBinary) and (ARequest.Body is TStream) then
        begin
          TStream(ARequest.Body).Position := 0;
          vRequest.SetWireBody(TStream(ARequest.Body), boBorrowed);
        end;
      end;

      vResponse.EngineContext := vContext;
      ProcessCommands(vRequest, vResponse);
      // a pull engine: the library sends the pieces after the handler returned
      if not FinishStream(vRequest, vResponse, False) then
      begin
        vType := vResponse.ContentType;
        vWire := vResponse.TakeWireStream;
        if vResponse.ContentType = '' then
          vResponse.ContentType := vType;
        vRequest.Finish;
        FillLibraryHead(AResponse, Self, vResponse, vResponse.StatusCode);
        if vWire <> nil then
          vLength := vWire.Size
        else
          vLength := 0;
        if vRequest.Method = amHEAD then
        begin
          vWire.Free;
          vText := HeadBytes(AResponse, vLength);
          AResponse.Connection.SendBytes(vText,
            procedure(const AConnection: ICrossConnection; const ASuccess: boolean)
            begin
              AConnection.Disconnect;
            end);
        end
        else if vWire = nil then
          AResponse.SendNoCompress(TBytes(nil))
        else
        begin
          vWire.Position := 0;
          // the library reads the stream while it sends: freed at the end
          AResponse.SendNoCompress(vWire, 0, vLength,
            procedure(const AConnection: ICrossConnection; const ASuccess: boolean)
            begin
              vWire.Free;
            end);
        end;
      end;
    except
      on E: Exception do
      begin
        vBody.Free;
        vBody := nil;
        AnswerFailure(AResponse, E);
      end;
    end;
  finally
    vBody.Free;
    vContext.Free;
    vRequest.Free;
    vResponse.Free;
  end;
end;

procedure TRALXSocketServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  if AValue then
  begin
    inherited SetActive(True);
    try
      StartServer;
    except
      StopServer;
      inherited SetActive(False);
      raise;
    end;
  end
  else
  begin
    // the open streams were closed by the caller of the setter
    StopServer;
    inherited SetActive(False);
  end;
end;

procedure TRALXSocketServer.SetSSL(const AValue: TRALXSocketSSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

function TRALXSocketServer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
begin
  SSL.CertFile := ACertificate.CertificateFile;
  SSL.KeyFile := ACertificate.PrivateKeyFile;
  SSL.CertPEM := ACertificate.CertificatePEM;
  SSL.KeyPEM := ACertificate.PrivateKeyPEM;
  SSL.Enabled := True;
  if Active then
    Result := tarRestartNeeded
  else
    Result := tarApplied;
end;

procedure TRALXSocketServer.StartServer;
var
  vInt, vCount: IntegerRAL;
  vDone: TEvent;
  vListening: boolean;
  vHttp: ICrossHttpServer;
begin
  vHttp := TCrossHttpServer.Create(FIoThreads, SSL.Enabled);
  vHttp.Addr := string(IPConfig.IPv4Bind);
  vHttp.Port := Port;
  vHttp.Compressible := False;
  vHttp.MaxPostDataSize := 0;
  vHttp.OnConnected := Connected;
  vHttp.OnRequest := HttpRequest;
  if SSL.Enabled then
  begin
    if SSL.CertPEM <> '' then
    begin
      vHttp.SetCertificate(string(SSL.CertPEM));
      vHttp.SetPrivateKey(string(SSL.KeyPEM));
    end
    else
    begin
      vHttp.SetCertificateFile(string(SSL.CertFile));
      vHttp.SetPrivateKeyFile(string(SSL.KeyFile));
    end;
  end;
  FHttp := vHttp;
  FLiveStreams := 0;
  if not FExecuteInIOThread then
  begin
    vCount := FPoolCount;
    if vCount <= 0 then
      vCount := TThread.ProcessorCount * 4;
    FQueue := TThreadedQueue<TRALXSocketWork>.Create(65536, INFINITE, 100);
    for vInt := 1 to vCount do
      FWorkers.Add(TRALXSocketWorker.Create(Self));
  end;
  vDone := TEvent.Create(nil, True, False, '');
  try
    vListening := False;
    vHttp.Start(
      procedure(const AListen: ICrossListen; const ASuccess: boolean)
      begin
        vListening := ASuccess;
        vDone.SetEvent;
      end);
    if (vDone.WaitFor(10000) <> wrSignaled) or not vListening then
      raise ERALException.Create('XSocket: the port ' + IntToStr(Port) + ' cannot be opened');
  finally
    vDone.Free;
  end;
end;

procedure TRALXSocketServer.StopServer;
var
  vInt: IntegerRAL;
  vWork: TRALXSocketWork;
begin
  if FQueue <> nil then
  begin
    for vInt := 0 to FWorkers.Count - 1 do
      TThread(FWorkers[vInt]).Terminate;
  end;
  if FHttp <> nil then
  begin
    FHttp.Stop;
    FHttp := nil;
  end;
  for vInt := 0 to FWorkers.Count - 1 do
  begin
    TThread(FWorkers[vInt]).WaitFor;
    TThread(FWorkers[vInt]).Free;
  end;
  FWorkers.Clear;
  if FQueue <> nil then
  begin
    while FQueue.QueueSize > 0 do
    begin
      vWork := nil;
      if FQueue.PopItem(vWork) <> wrSignaled then
        Break;
      vWork.Free;
    end;
    FreeAndNil(FQueue);
  end;
end;

procedure TRALXSocketServer.StreamEnded;
begin
  RALAtomicDec(FLiveStreams);
end;

class function TRALXSocketServer.SupportsLiveStream: boolean;
begin
  Result := True;
end;

function TRALXSocketServer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

end.
