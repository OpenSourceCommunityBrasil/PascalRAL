{ AdvancedHTTPServer engine (Free Pascal 3.3+ only: the library's handlers are
  anonymous functions). One THTTPServer of the library runs on a background
  thread (epoll on Linux, IOCP on Windows); every request goes through RAL's
  pipeline. Without FPC 3.3 the unit compiles empty.

  The library keeps its stop flag in a unit variable (ShutdownRequested), so
  one process runs one of these servers at a time. }
unit RALAdvancedHTTPServer;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$IFDEF RALHasAnonMethods}
    {$DEFINE RALAdvancedHTTP}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}

interface

{$IFDEF RALAdvancedHTTP}

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  AdvancedHTTPServer,
  RALTypes, RALConsts, RALTools, RALServer, RALRequest, RALResponse, RALParams,
  RALCustomObjects, RALResponseStream, RALMIMETypes;

type
  TRALAdvancedHTTPServer = class;

  /// TLS of the engine: the certificate and its key as PEM files.
  TRALAdvancedHTTPSSL = class(TRALSSL)
  private
    FCertFile: StringRAL;
    FKeyFile: StringRAL;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    /// Certificate (PEM, with its chain).
    property CertFile: StringRAL read FCertFile write FCertFile;
    /// Private key of CertFile (PEM, not encrypted).
    property KeyFile: StringRAL read FKeyFile write FKeyFile;
  end;

  /// The thread the library's blocking listen call runs on.
  TRALAdvancedHTTPThread = class(TThread)
  private
    /// Address the library listens on ("host:port").
    FAddress: string;
    /// Certificate of a TLS listener.
    FCertFile: string;
    /// Message of the exception that ended the listen call.
    FError: string;
    /// Key of a TLS listener.
    FKeyFile: string;
    /// The RAL server whose requests are counted.
    FOwner: TRALAdvancedHTTPServer;
    /// The library's server.
    FServer: THTTPServer;
    /// The listener serves TLS.
    FTLS: boolean;
  protected
    procedure Execute; override;
  public
    /// A thread that listens for AOwner on AServer; started by the caller.
    constructor Create(AOwner: TRALAdvancedHTTPServer; AServer: THTTPServer);
  end;

  { RAL server over AdvancedHTTPServer. Requests run on the library's handler
    pool (PoolCount threads); the answer goes out with a Content-Length, or
    chunked when a handler streams (Send). }
  TRALAdvancedHTTPServer = class(TRALServer)
  private
    /// Idle seconds before a kept connection is closed.
    FConnectionTimeout: IntegerRAL;
    /// The library's server, alive while the engine is active.
    FHTTP: THTTPServer;
    /// Requests inside the handler now.
    FInFlight: LongInt;
    /// Largest request body, in bytes.
    FMaxBodySize: Int64RAL;
    /// Threads of the library's event loop and handler pool; 0 = one per processor.
    FPoolCount: IntegerRAL;
    /// TCP_NODELAY on the accepted sockets.
    FTcpNoDelay: boolean;
    /// The thread that listens.
    FThread: TRALAdvancedHTTPThread;

    /// Copies the head of AResponse into the writer's headers.
    procedure FillHead(AWriter: TResponseWriter; AResponse: TRALResponse);
    function GetSSL: TRALAdvancedHTTPSSL;
    /// The library's handler: one request through the RAL pipeline.
    procedure Serve(AWriter: TResponseWriter; ARequest: TRequest);
    /// Answers AWriter with a 500 when nothing was sent.
    procedure ServeError(AWriter: TResponseWriter; AError: Exception);
    procedure SetSSL(const AValue: TRALAdvancedHTTPSSL);
    /// Starts the listener and waits until it accepts.
    procedure StartListening;
    /// Stops the listener and frees the library's server.
    procedure StopListening;
    /// Sends a whole answer: the status, the length and AStream as the body.
    procedure WriteAnswer(AWriter: TResponseWriter; AStatus: IntegerRAL; AStream: TStream; AHeadOnly: boolean);
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
    /// Seconds a kept connection may stay idle before it is closed.
    property ConnectionTimeout: IntegerRAL read FConnectionTimeout write FConnectionTimeout default 30;
    { Largest request body in bytes; a bigger one is answered 413 by the library.
      The library's own default is 32 MB. }
    property MaxBodySize: Int64RAL read FMaxBodySize write FMaxBodySize default 33554432;
    { Threads of the library's event loop and of its handler pool (the pool has at
      least two); 0 = one per processor. }
    property PoolCount: IntegerRAL read FPoolCount write FPoolCount default 0;
    /// TLS settings (PEM files).
    property SSL: TRALAdvancedHTTPSSL read GetSSL write SetSSL;
    { The library never sets TCP_NODELAY and answers with two sends (head, then
      body), which Nagle plus the client's delayed acknowledgement stall for ~40 ms.
      On: it is set on the accepted socket, found through the library's own
      structures (checked to be a stream socket first). }
    property TcpNoDelay: boolean read FTcpNoDelay write FTcpNoDelay default True;
  end;

{$ENDIF}

implementation

{$IFDEF RALAdvancedHTTP}

uses
  {$IFDEF RALWindows}
    WinSock2,
  {$ELSE}
    BaseUnix,
  {$ENDIF}
  // fpgetsockopt, fpsetsockopt, IPPROTO_TCP, TCP_NODELAY, SO_TYPE
  Sockets;

const
  /// Bytes a body is read and written in.
  cPiece = 65536;
  /// Largest answer sent by the library's own Finish, in one piece.
  cBufferedMax = 262144;
  /// The library accepts one server at a time (see the unit's header).
  cOneServer = 'Only one TRALAdvancedHTTPServer can be active in a process (the library keeps a single stop flag).';
  /// Message when the listener never accepted.
  cNotListening = 'TRALAdvancedHTTPServer could not listen on port %d%s.';

type
  /// What a streamed answer writes through: the library's writer and request.
  TRALAdvancedHTTPContext = class
  public
    /// The library's request.
    Request: TRequest;
    /// The RAL server.
    Server: TRALAdvancedHTTPServer;
    /// The library's writer.
    Writer: TResponseWriter;
  end;

  { Writer of a live answer: the pieces go through the library's own chunked
    framing (FramesItself), which Write applies to a keep-alive answer. }
  TRALAdvancedHTTPStreamWriter = class(TRALStreamWriter)
  private
    /// Where the answer goes.
    FContext: TRALAdvancedHTTPContext;
  public
    /// A writer for AContext.
    constructor Create(AContext: TRALAdvancedHTTPContext);

    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

var
  /// Servers active now (the library allows one).
  GActive: LongInt = 0;
  /// Output was pointed at NUL (a program without a console).
  GNulOutput: boolean = False;

/// The bytes of AText as the library's string (no code page conversion).
function ToLib(const AText: StringRAL): string;
begin
  SetLength(Result, Length(AText));
  if Length(AText) > 0 then
    Move(Pointer(AText)^, Pointer(Result)^, Length(AText));
end;

/// The bytes of the library's AText as RAL's string (no code page conversion).
function FromLib(const AText: string): StringRAL;
begin
  SetLength(Result, Length(AText));
  if Length(AText) > 0 then
    Move(Pointer(AText)^, Pointer(Result)^, Length(AText));
end;

/// Sets TCP_NODELAY on the socket behind AWriter; false when it cannot be found.
{ TResponseWriter keeps its connection in the first field after the class
  pointer, and TClientConnection starts with the socket: both private, read by
  position (library of 2026). The descriptor is checked to be a stream socket
  before it is changed, so a layout that moved does nothing. }
procedure SetNoDelay(AWriter: TResponseWriter);
var
  vConn: Pointer;
  vSock: LongInt;
  vValue, vType: LongInt;
  vLen: TSockLen;
begin
  vConn := PPointer(PByte(AWriter) + SizeOf(Pointer))^;
  if vConn = nil then
    Exit;
  vSock := PLongInt(vConn)^;
  if vSock < 0 then
    Exit;
  vLen := SizeOf(vType);
  vType := 0;
  if (fpgetsockopt(vSock, SOL_SOCKET, SO_TYPE, @vType, @vLen) <> 0) or
     (vType <> SOCK_STREAM) then
    Exit;
  vValue := 1;
  fpsetsockopt(vSock, IPPROTO_TCP, TCP_NODELAY, @vValue, SizeOf(vValue));
end;

/// Status codes whose answer has no body.
function NoBody(AStatus: IntegerRAL): boolean;
begin
  Result := (AStatus < 200) or (AStatus = 204) or (AStatus = 304);
end;

{ TRALAdvancedHTTPSSL }

procedure TRALAdvancedHTTPSSL.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TRALAdvancedHTTPSSL then
  begin
    TRALAdvancedHTTPSSL(Dest).FCertFile := FCertFile;
    TRALAdvancedHTTPSSL(Dest).FKeyFile := FKeyFile;
  end;
end;

{ TRALAdvancedHTTPThread }

constructor TRALAdvancedHTTPThread.Create(AOwner: TRALAdvancedHTTPServer; AServer: THTTPServer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FServer := AServer;
end;

procedure TRALAdvancedHTTPThread.Execute;
var
  vInt: LongInt;
begin
  try
    if FTLS then
      FServer.ListenAndServeTLS(FAddress, FCertFile, FKeyFile)
    else
      FServer.ListenAndServe(FAddress);
  except
    on E: Exception do
      FError := E.Message;
  end;
  { the library's own Shutdown does nothing once the flag is set (so it never
    closes the listening socket on Windows): the flag goes back down for the
    real one. Handlers still running finish first, then every connection left
    is closed at once (ConnectionTimeout 0). }
  vInt := 0;
  while (FOwner.FInFlight > 0) and (vInt < 300) do
  begin
    Sleep(10);
    Inc(vInt);
  end;
  { the library still sends the buffered answer after the handler returned }
  Sleep(50);
  FServer.ConnectionTimeout := 0;
  ShutdownRequested := False;
  FServer.Shutdown;
end;

{ TRALAdvancedHTTPStreamWriter }

constructor TRALAdvancedHTTPStreamWriter.Create(AContext: TRALAdvancedHTTPContext);
begin
  inherited Create;
  FContext := AContext;
  { Write frames a keep-alive answer in chunks itself (HTTP/1.0 is closed to end) }
  FramesItself := True;
end;

procedure TRALAdvancedHTTPStreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo;
  AStatusCode: IntegerRAL);
begin
  FContext.Server.FillHead(FContext.Writer, TRALResponse(AResponse));
  { a HEAD answer has no body: a length keeps the library from chunking it }
  if HeadOnly then
    FContext.Writer.Header.SetValue('Content-Length', '0');
  if FContext.Server.FTcpNoDelay then
    SetNoDelay(FContext.Writer);
  FContext.Writer.WriteHeader(AStatusCode);
end;

function TRALAdvancedHTTPStreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
begin
  try
    FContext.Writer.Write(ABuffer, ASize);
    Result := True;
  except
    Result := False;
  end;
end;

{ TRALAdvancedHTTPServer }

constructor TRALAdvancedHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionTimeout := 30;
  FMaxBodySize := 33554432;
  FTcpNoDelay := True;
  Port := 8000;
  SetEngine('AdvancedHTTP');
end;

destructor TRALAdvancedHTTPServer.Destroy;
begin
  Active := False;
  inherited;
end;

function TRALAdvancedHTTPServer.CreateRALSSL: TRALSSL;
begin
  Result := TRALAdvancedHTTPSSL.Create;
end;

function TRALAdvancedHTTPServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  Result := nil;
  {$IFDEF RALWindows}
  { the library writes a chunked body over TLS on Windows without the chunk
    framing: such an answer stays buffered }
  if SSL.Enabled then
    Exit;
  {$ENDIF}
  if AResponse.EngineContext is TRALAdvancedHTTPContext then
  begin
    Result := TRALAdvancedHTTPStreamWriter.Create(TRALAdvancedHTTPContext(AResponse.EngineContext));
    Result.HeadOnly := ARequest.Method = amHEAD;
  end;
end;

procedure TRALAdvancedHTTPServer.FillHead(AWriter: TResponseWriter;
  AResponse: TRALResponse);
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vCookies: TStringList;
begin
  AWriter.Header.Clear;
  if AResponse.ContentType <> '' then
    AWriter.Header.AddValue('Content-Type', ToLib(RALSafeHeaderText(AResponse.ContentType)));
  if AResponse.ContentEncoding <> '' then
    AWriter.Header.AddValue('Content-Encoding', ToLib(RALSafeHeaderText(AResponse.ContentEncoding)));
  if AResponse.ContentDisposition <> '' then
    AWriter.Header.AddValue('Content-Disposition', ToLib(RALSafeHeaderText(AResponse.ContentDisposition)));
  if AResponse.ContentEncription <> '' then
    AWriter.Header.AddValue('Content-Encription', ToLib(RALSafeHeaderText(AResponse.ContentEncription)));
  for vInt := 0 to AResponse.Params.Count - 1 do
  begin
    vParam := AResponse.Params.Index[vInt];
    if (vParam.Kind = rpkHEADER) and (vParam.ParamName <> '') and
       (not RALSameName(vParam.ParamName, 'Content-Length')) and
       (not RALSameName(vParam.ParamName, 'Transfer-Encoding')) then
      AWriter.Header.AddValue(ToLib(vParam.ParamName), ToLib(RALSafeHeaderText(vParam.AsString)));
  end;
  vCookies := TStringList.Create;
  try
    AResponse.GetParamsCookies(vCookies, IncMinute(Now, CookieLife));
    for vInt := 0 to vCookies.Count - 1 do
      AWriter.Header.AddValue('Set-Cookie', ToLib(StringRAL(vCookies[vInt])));
  finally
    vCookies.Free;
  end;
end;

function TRALAdvancedHTTPServer.GetSSL: TRALAdvancedHTTPSSL;
begin
  Result := TRALAdvancedHTTPSSL(GetDefaultSSL);
end;

function TRALAdvancedHTTPServer.GetTLSCertificate(out ACertificate: TRALTLSCertificate): boolean;
begin
  RALClearTLSCertificate(ACertificate);
  ACertificate.CertificateFile := SSL.CertFile;
  ACertificate.PrivateKeyFile := SSL.KeyFile;
  Result := SSL.CertFile <> '';
end;

function TRALAdvancedHTTPServer.IPv6IsImplemented: boolean;
begin
  Result := False;
end;

procedure TRALAdvancedHTTPServer.Serve(AWriter: TResponseWriter; ARequest: TRequest);
var
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vContext: TRALAdvancedHTTPContext;
  vStream: TStream;
  vInt, vPos: IntegerRAL;
  vName, vValue, vCookies, vAddr: StringRAL;
  vHash: QWord;
begin
  InterLockedIncrement(FInFlight);
  vRequest := CreateRequest;
  vResponse := CreateResponse;
  vContext := TRALAdvancedHTTPContext.Create;
  try
    try
      vRequest.Query := FromLib(ARequest.URL);
      vRequest.Method := HTTPMethodToRALMethod(FromLib(ARequest.Method));
      vRequest.Protocol := FromLib(ARequest.Proto);
      vRequest.ProtocolVersion := rhv11;
      if ARequest.TLS then
        vRequest.HttpVersion := 'HTTPS'
      else
        vRequest.HttpVersion := 'HTTP';
      { "ip:port" or "[ipv6]:port" }
      vAddr := FromLib(ARequest.RemoteAddr);
      vInt := Length(vAddr);
      while (vInt > 0) and (vAddr[vInt] <> ':') do
        Dec(vInt);
      if vInt > 0 then
      begin
        vRequest.ClientInfo.IP := Copy(vAddr, 1, vInt - 1);
        vRequest.ClientInfo.Port := StrToIntDef(string(Copy(vAddr, vInt + 1, MaxInt)), 0);
        if (Length(vRequest.ClientInfo.IP) > 1) and (vRequest.ClientInfo.IP[1] = '[') then
          vRequest.ClientInfo.IP := Copy(vRequest.ClientInfo.IP, 2, Length(vRequest.ClientInfo.IP) - 2);
      end
      else
        vRequest.ClientInfo.IP := vAddr;
      { the library gives no connection: ip and port of the peer name one }
      vHash := 14695981039346656037;
      for vInt := 1 to Length(vAddr) do
        vHash := (vHash xor Ord(vAddr[vInt])) * 1099511628211;
      vRequest.ClientInfo.ConnectionID := Int64(vHash);

      vCookies := '';
      for vInt := 0 to ARequest.Header.Count - 1 do
      begin
        vValue := FromLib(ARequest.Header[vInt]);
        vPos := Pos(':', string(vValue));
        if vPos <= 0 then
          Continue;
        vName := Trim(Copy(vValue, 1, vPos - 1));
        vValue := Trim(Copy(vValue, vPos + 1, MaxInt));
        if RALSameName(vName, 'Host') then
          vRequest.Host := vValue
        else if RALSameName(vName, 'Content-Type') then
          vRequest.ContentType := vValue
        else if RALSameName(vName, 'Content-Encoding') then
          vRequest.ContentEncoding := vValue
        else if RALSameName(vName, 'Accept-Encoding') then
          vRequest.AcceptEncoding := vValue
        else if RALSameName(vName, 'Content-Encription') then
          vRequest.ContentEncription := vValue
        else if RALSameName(vName, 'Accept-Encription') then
          vRequest.AcceptEncription := vValue
        else if RALSameName(vName, 'User-Agent') then
          vRequest.ClientInfo.UserAgent := vValue
        else if RALSameName(vName, 'Cookie') then
        begin
          if vCookies <> '' then
            vCookies := vCookies + '; ';
          vCookies := vCookies + vValue;
        end;
        vRequest.Params.AddParam(vName, vValue, rpkHEADER);
      end;
      if vCookies <> '' then
        vRequest.AddCookies(vCookies);
      vRequest.ContentSize := Length(ARequest.BodyBytes);
      DecodeAuth(vRequest);

      ValidateRequest(vRequest, vResponse);
      if (vResponse.StatusCode < HTTP_BadRequest) and (Length(ARequest.BodyBytes) > 0) then
        vRequest.SetWireBody(@ARequest.BodyBytes[0], Length(ARequest.BodyBytes));

      vContext.Request := ARequest;
      vContext.Server := Self;
      vContext.Writer := AWriter;
      vResponse.EngineContext := vContext;
      ProcessCommands(vRequest, vResponse);
      if not FinishStream(vRequest, vResponse) then
      begin
        vStream := vResponse.TakeWireStream;
        try
          vRequest.Finish;
          FillHead(AWriter, vResponse);
          WriteAnswer(AWriter, vResponse.StatusCode, vStream, vRequest.Method = amHEAD);
        finally
          vStream.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        ServeError(AWriter, E);
        if Assigned(OnServerError) then
          OnServerError(E)
        else if RaiseError then
          raise;
      end;
    end;
  finally
    vRequest.Free;
    vResponse.Free;
    vContext.Free;
    InterLockedDecrement(FInFlight);
  end;
end;

procedure TRALAdvancedHTTPServer.ServeError(AWriter: TResponseWriter; AError: Exception);
var
  vText: string;
begin
  if AWriter.HeadersSent then
    Exit;
  vText := ToLib(ErrorText(AError));
  AWriter.Header.Clear;
  AWriter.Header.AddValue('Content-Type', string(rctTEXTPLAIN));
  AWriter.Header.AddValue('Content-Length', IntToStr(Length(vText)));
  AWriter.WriteHeader(HTTP_InternalError);
  if vText <> '' then
    AWriter.Write(Pointer(vText)^, Length(vText));
end;

procedure TRALAdvancedHTTPServer.WriteAnswer(AWriter: TResponseWriter; AStatus: IntegerRAL;
  AStream: TStream; AHeadOnly: boolean);
var
  vSize: Int64RAL;
  vBody: ansistring;
  vBuffer: array of Byte;
  vRead: IntegerRAL;
begin
  vSize := 0;
  if AStream <> nil then
    vSize := AStream.Size;
  if not NoBody(AStatus) then
    AWriter.Header.SetValue('Content-Length', IntToStr(vSize));
  if FTcpNoDelay then
    SetNoDelay(AWriter);
  if (AStatus = HTTP_OK) and (vSize <= cBufferedMax) and (not AHeadOnly) then
  begin
    { the library sends this one itself when the handler returns, which is the
      last moment before it can take the connection's next request }
    if vSize > 0 then
    begin
      SetLength(vBody, vSize);
      AStream.Position := 0;
      AStream.ReadBuffer(Pointer(vBody)^, vSize);
      AWriter.SetBufferedBody(vBody);
    end;
    Exit;
  end;
  AWriter.WriteHeader(AStatus);
  if (vSize = 0) or AHeadOnly or NoBody(AStatus) then
    Exit;
  AStream.Position := 0;
  if AStream is TCustomMemoryStream then
    AWriter.Write(TCustomMemoryStream(AStream).Memory^, vSize)
  else
  begin
    SetLength(vBuffer, cPiece);
    repeat
      vRead := AStream.Read(vBuffer[0], cPiece);
      if vRead > 0 then
        AWriter.Write(vBuffer[0], vRead);
    until vRead <= 0;
  end;
end;

procedure TRALAdvancedHTTPServer.SetActive(const AValue: boolean);
begin
  if AValue = Active then
    Exit;
  if AValue then
  begin
    if GActive > 0 then
      raise Exception.Create(cOneServer);
    inherited SetActive(True);
    try
      StartListening;
    except
      StopListening;
      inherited SetActive(False);
      raise;
    end;
  end
  else
  begin
    { the base closes the open streams and refuses new requests first }
    inherited SetActive(False);
    StopListening;
  end;
end;

procedure TRALAdvancedHTTPServer.SetSSL(const AValue: TRALAdvancedHTTPSSL);
begin
  GetDefaultSSL.Assign(AValue);
end;

function TRALAdvancedHTTPServer.SetTLSCertificate(
  const ACertificate: TRALTLSCertificate): TRALTLSApplyResult;
begin
  if ACertificate.CertificateFile = '' then
  begin
    Result := tarUnsupported;
    Exit;
  end;
  SSL.CertFile := ACertificate.CertificateFile;
  SSL.KeyFile := ACertificate.PrivateKeyFile;
  SSL.Enabled := True;
  { the library reads the files when it starts }
  if Active then
    Result := tarRestartNeeded
  else
    Result := tarApplied;
end;

procedure TRALAdvancedHTTPServer.StartListening;
var
  vBind, vHost: string;
  vTry: IntegerRAL;
  vProbe: TSocket;
  vAddr: TInetSockAddr;
  vUp: boolean;
begin
  {$IFDEF RALWindows}
  { the library writes progress to the console; without one that is an I/O error }
  if (not IsConsole) and (not GNulOutput) then
  begin
    AssignFile(Output, 'NUL');
    Rewrite(Output);
    GNulOutput := True;
  end;
  {$ENDIF}
  ShutdownRequested := False;
  FHTTP := THTTPServer.Create;
  FHTTP.WorkerCount := FPoolCount;
  if FHTTP.WorkerCount <= 0 then
    FHTTP.WorkerCount := TThread.ProcessorCount;
  FHTTP.ConnectionTimeout := FConnectionTimeout;
  FHTTP.MaxBodyBytes := FMaxBodySize;
  { the route is in place before anything can connect }
  FHTTP.HandleFunc('/',
    procedure(AWriter: TResponseWriter; ARequest: TRequest)
    begin
      Serve(AWriter, ARequest);
    end);
  vBind := string(IPConfig.IPv4Bind);
  FThread := TRALAdvancedHTTPThread.Create(Self, FHTTP);
  FThread.FAddress := vBind + ':' + IntToStr(Port);
  FThread.FTLS := SSL.Enabled;
  FThread.FCertFile := string(SSL.CertFile);
  FThread.FKeyFile := string(SSL.KeyFile);
  InterLockedIncrement(GActive);
  FThread.Start;

  { listening is known by connecting to it: the library gives no signal }
  vHost := vBind;
  if (vHost = '') or (vHost = '0.0.0.0') then
    vHost := '127.0.0.1';
  vUp := False;
  vTry := 0;
  while (not vUp) and (not FThread.Finished) and (vTry < 100) do
  begin
    vProbe := fpsocket(AF_INET, SOCK_STREAM, 0);
    if vProbe <> TSocket(-1) then
    begin
      FillChar(vAddr, SizeOf(vAddr), 0);
      vAddr.sin_family := AF_INET;
      vAddr.sin_port := htons(Port);
      vAddr.sin_addr := StrToNetAddr(vHost);
      vUp := fpconnect(vProbe, @vAddr, SizeOf(vAddr)) = 0;
      CloseSocket(vProbe);
    end;
    if not vUp then
    begin
      Sleep(50);
      Inc(vTry);
    end;
  end;
  if not vUp then
  begin
    if FThread.FError <> '' then
      vHost := ': ' + FThread.FError
    else
      vHost := '';
    raise Exception.CreateFmt(cNotListening, [Port, vHost]);
  end;
end;

procedure TRALAdvancedHTTPServer.StopListening;
begin
  if FThread <> nil then
  begin
    ShutdownRequested := True;
    FThread.WaitFor;
    FreeAndNil(FThread);
    InterLockedDecrement(GActive);
  end;
  FreeAndNil(FHTTP);
  ShutdownRequested := False;
end;

class function TRALAdvancedHTTPServer.SupportsLiveStream: boolean;
begin
  Result := True;
end;

function TRALAdvancedHTTPServer.TLSProvisioning: TRALTLSProvisioning;
begin
  Result := tpPEMFiles;
end;

{$ENDIF}

end.
