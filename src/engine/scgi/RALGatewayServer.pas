{ Base of the engines behind a web server that speaks a simple gateway protocol (SCGI,
  uwsgi): one request per connection, answered as it is written until it closes. }
unit RALGatewayServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALServer, RALRequest, RALResponse, RALParams,
  RALCustomObjects, RALResponseStream, RALResponsePages, RALStream, RALSocket;

type
  TRALGatewayServer = class;

  /// One connection from the web server: one request, then it closes.
  TRALGatewayConnection = class(TThread)
  private
    /// Socket to the web server.
    FChannel: TRALSocketChannel;
    /// The RAL server.
    FServer: TRALGatewayServer;
  protected
    procedure Execute; override;
  public
    /// A connection over ASocket (owned) for AServer.
    constructor Create(AServer: TRALGatewayServer; ASocket: TRALSocket);
    destructor Destroy; override;

    { Reads exactly ACount bytes, waiting up to the server's ReadTimeout for
      each piece; False when the connection closed or went silent }
    function ReadExact(var ABuffer; ACount: IntegerRAL): boolean;
    /// Writes ACount bytes; False when the connection broke.
    function WriteBytes(const ABuffer; ACount: IntegerRAL): boolean;

    /// Socket to the web server.
    property Channel: TRALSocketChannel read FChannel;
  end;

  { The listener of a gateway engine: accepts the web server's connections
    and gives each a thread }
  TRALGatewayServer = class(TRALServer)
  private
    /// Thread that accepts connections.
    FAcceptThread: TThread;
    /// Open connections.
    FConnections: TList;
    /// Guards FConnections.
    FConnLock: TCriticalSection;
    /// Listening socket.
    FListener: TRALSocket;
    FMaxConnections: IntegerRAL;
    FMaxHeaderSize: IntegerRAL;
    FReadTimeout: IntegerRAL;

    /// Takes AConnection out of the list.
    procedure ConnectionDone(AConnection: TRALGatewayConnection);
  protected
    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; override;
    { Reads the block of CGI variables of the protocol from AConnection into
      AEnv (NAME=value lines); False when it is malformed or the connection
      closed }
    function ReadEnvironment(AConnection: TRALGatewayConnection; AEnv: TStrings): boolean; virtual; abstract;
    procedure SetActive(const AValue: boolean); override;
    /// The first line of the answer, without the line break.
    function StatusLine(AStatusCode: IntegerRAL): RawByteString; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { The reason phrase of AStatusCode for a status line ('OK', 'Not Found'),
      'Unknown' for a code it does not know }
    class function ReasonPhrase(AStatusCode: IntegerRAL): string;
    { Builds the status line and headers of an answer; Content-Length is written
      when ALength is not negative. }
    function ResponseHead(AResponse: TRALResponse; AStatusCode: IntegerRAL;
      ALength: Int64RAL): RawByteString;
    class function SupportsLiveStream: boolean; override;
  published
    /// Most connections from the web server at once; 0 is no limit.
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 0;
    /// Largest block of CGI variables accepted, in bytes.
    property MaxHeaderSize: IntegerRAL read FMaxHeaderSize write FMaxHeaderSize default 65536;
    /// Milliseconds a connection may stay silent in the middle of a request.
    property ReadTimeout: IntegerRAL read FReadTimeout write FReadTimeout default 30000;
  end;

implementation

type
  /// What a streamed answer writes through.
  TRALGatewayContext = class
  public
    /// Connection of the request.
    Connection: TRALGatewayConnection;
    /// Status and headers went out.
    HeadSent: boolean;
    /// The RAL server.
    Server: TRALGatewayServer;
  end;

  { Writer of a streamed answer: the bytes as they are, the end of the
    connection ends the body }
  TRALGatewayStreamWriter = class(TRALStreamWriter)
  private
    /// Request the stream belongs to.
    FContext: TRALGatewayContext;
  public
    /// A writer for AContext.
    constructor Create(AContext: TRALGatewayContext);

    function Alive: boolean; override;
    procedure WriteEnd(AAborted: boolean); override;
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

  /// Thread that accepts the web server's connections.
  TRALGatewayAccept = class(TThread)
  private
    /// Server it accepts for.
    FServer: TRALGatewayServer;
  protected
    procedure Execute; override;
  public
    /// The accept thread of AServer, started.
    constructor Create(AServer: TRALGatewayServer);
  end;

{ TRALGatewayStreamWriter }

constructor TRALGatewayStreamWriter.Create(AContext: TRALGatewayContext);
begin
  inherited Create;
  FContext := AContext;
  FramesItself := True; // the end of the connection ends the body: no chunks
end;

function TRALGatewayStreamWriter.Alive: boolean;
begin
  Result := not FContext.Connection.Terminated;
end;

procedure TRALGatewayStreamWriter.WriteEnd(AAborted: boolean);
begin
  // the connection closes after the request; a cut answer closes it too
  if AAborted then
    FContext.Connection.Channel.Close;
end;

procedure TRALGatewayStreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL);
var
  vHead: RawByteString;
begin
  vHead := FContext.Server.ResponseHead(TRALResponse(AResponse), AStatusCode, -1);
  FContext.HeadSent := True;
  FContext.Connection.WriteBytes(Pointer(vHead)^, Length(vHead));
end;

function TRALGatewayStreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := FContext.Connection.WriteBytes(ABuffer, ASize);
end;

{ TRALGatewayAccept }

constructor TRALGatewayAccept.Create(AServer: TRALGatewayServer);
begin
  FServer := AServer;
  inherited Create(False);
end;

procedure TRALGatewayAccept.Execute;
var
  vSocket: TRALSocket;
  vConn: TRALGatewayConnection;
begin
  while not Terminated do
  begin
    try
      if (FServer.FListener.WaitFor(RALPollIn, 200) and RALPollIn) = 0 then
        Continue;
      vSocket := FServer.FListener.Accept;
      if vSocket = nil then
        Continue;
      FServer.FConnLock.Acquire;
      try
        if (FServer.FMaxConnections > 0) and
           (FServer.FConnections.Count >= FServer.FMaxConnections) then
        begin
          vSocket.Free;
          Continue;
        end;
        vConn := TRALGatewayConnection.Create(FServer, vSocket);
        FServer.FConnections.Add(vConn);
      finally
        FServer.FConnLock.Release;
      end;
      vConn.Start;
    except
      // a refused accept must not end the listener
      if not Terminated then
        Sleep(10);
    end;
  end;
end;

{ TRALGatewayConnection }

constructor TRALGatewayConnection.Create(AServer: TRALGatewayServer; ASocket: TRALSocket);
begin
  FServer := AServer;
  FChannel := TRALSocketChannel.Create(ASocket, True);
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TRALGatewayConnection.Destroy;
begin
  FServer.ConnectionDone(Self);
  FreeAndNil(FChannel);
  inherited;
end;

function TRALGatewayConnection.ReadExact(var ABuffer; ACount: IntegerRAL): boolean;
var
  vGot, vRead: IntegerRAL;
begin
  Result := False;
  vGot := 0;
  while vGot < ACount do
  begin
    vRead := FChannel.ReadWait(PByte(NativeUInt(@ABuffer) + NativeUInt(vGot))^, ACount - vGot,
      FServer.ReadTimeout);
    if vRead <= 0 then
      Exit;
    Inc(vGot, vRead);
  end;
  Result := True;
end;

function TRALGatewayConnection.WriteBytes(const ABuffer; ACount: IntegerRAL): boolean;
begin
  Result := True;
  if ACount <= 0 then
    Exit;
  try
    FChannel.WriteAll(ABuffer, ACount, FServer.ReadTimeout);
  except
    Result := False;
  end;
end;

procedure TRALGatewayConnection.Execute;
var
  vEnv: TStringList;
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vContext: TRALGatewayContext;
  vName, vValue, vPath: string;
  vInt: IntegerRAL;
  vLength: Int64RAL;
  vBody, vHeadText, vScratch: RawByteString;
  vStream: TStream;
  vRead: IntegerRAL;
  vSize: Int64RAL;
begin
  vEnv := TStringList.Create;
  try
    try
      if not FServer.ReadEnvironment(Self, vEnv) then
        Exit;
      vLength := StrToInt64Def(vEnv.Values['CONTENT_LENGTH'], 0);
      vRequest := FServer.CreateRequest;
      vResponse := FServer.CreateResponse;
      vContext := TRALGatewayContext.Create;
      try
        vPath := vEnv.Values['REQUEST_URI'];
        if vPath = '' then
        begin
          vPath := vEnv.Values['SCRIPT_NAME'] + vEnv.Values['PATH_INFO'];
          if vEnv.Values['QUERY_STRING'] <> '' then
            vPath := vPath + '?' + vEnv.Values['QUERY_STRING'];
        end;
        if vPath = '' then
          vPath := '/';
        vRequest.Query := StringRAL(vPath);
        vRequest.Method := HTTPMethodToRALMethod(StringRAL(vEnv.Values['REQUEST_METHOD']));
        vRequest.Protocol := StringRAL(vEnv.Values['SERVER_PROTOCOL']);
        vRequest.Host := StringRAL(vEnv.Values['HTTP_HOST']);
        if SameText(vEnv.Values['HTTPS'], 'on') or (vEnv.Values['REQUEST_SCHEME'] = 'https') then
          vRequest.HttpVersion := 'HTTPS'
        else
          vRequest.HttpVersion := 'HTTP';
        vRequest.ClientInfo.IP := StringRAL(vEnv.Values['REMOTE_ADDR']);
        vRequest.ClientInfo.Port := StrToIntDef(vEnv.Values['REMOTE_PORT'], 0);
        vRequest.ClientInfo.UserAgent := StringRAL(vEnv.Values['HTTP_USER_AGENT']);
        vRequest.ContentType := StringRAL(vEnv.Values['CONTENT_TYPE']);
        vRequest.ContentSize := vLength;
        vRequest.AcceptEncoding := StringRAL(vEnv.Values['HTTP_ACCEPT_ENCODING']);
        vRequest.ContentEncoding := StringRAL(vEnv.Values['HTTP_CONTENT_ENCODING']);
        vRequest.AcceptEncription := StringRAL(vEnv.Values['HTTP_ACCEPT_ENCRIPTION']);
        vRequest.ContentEncription := StringRAL(vEnv.Values['HTTP_CONTENT_ENCRIPTION']);
        { the headers alone, as CGI names them (HTTP_X_Y = X-Y) }
        for vInt := 0 to vEnv.Count - 1 do
        begin
          vName := vEnv.Names[vInt];
          vValue := vEnv.ValueFromIndex[vInt];
          if Copy(vName, 1, 5) = 'HTTP_' then
            Delete(vName, 1, 5)
          else if (vName <> 'CONTENT_TYPE') and (vName <> 'CONTENT_LENGTH') then
            Continue;
          vRequest.Params.AddParam(StringRAL(StringReplace(vName, '_', '-', [rfReplaceAll])),
            StringRAL(vValue), rpkHEADER);
        end;
        vRequest.AddCookies(vRequest.Params.GetKind['Cookie', rpkHEADER].AsString);
        FServer.DecodeAuth(vRequest);

        { judged before the body is read: a refused request (size, address)
          costs no read }
        FServer.ValidateRequest(vRequest, vResponse);
        if vResponse.StatusCode < HTTP_BadRequest then
        begin
          if vLength > 0 then
          begin
            SetLength(vBody, IntegerRAL(vLength));
            if not ReadExact(vBody[1], IntegerRAL(vLength)) then
              Exit;
          end;
          vRequest.SetWireBody(vBody);
        end;
        vContext.Connection := Self;
        vContext.Server := FServer;
        vResponse.EngineContext := vContext;
        try
          FServer.ProcessCommands(vRequest, vResponse);
          if not FServer.FinishStream(vRequest, vResponse) then
          begin
            vStream := vResponse.TakeWireStream;
            try
              vRequest.Finish;
              if vStream <> nil then
                vSize := vStream.Size
              else
                vSize := 0;
              vHeadText := FServer.ResponseHead(vResponse, vResponse.StatusCode, vSize);
              WriteBytes(Pointer(vHeadText)^, Length(vHeadText));
              if (vStream <> nil) and (vRequest.Method <> amHEAD) then
              begin
                vStream.Position := 0;
                SetLength(vScratch, RALWorkBufferSize(vStream));
                repeat
                  vRead := vStream.Read(vScratch[1], Length(vScratch));
                  if (vRead > 0) and not WriteBytes(vScratch[1], vRead) then
                    Break;
                until vRead <= 0;
              end;
            finally
              vStream.Free;
            end;
          end;
        except
          on E: Exception do
            { nothing sent yet: a 500; else the answer ends cut }
            if not vContext.HeadSent then
            begin
              vHeadText := FServer.StatusLine(HTTP_InternalError) +
                #13#10'Content-Type: text/plain'#13#10'Content-Length: 0'#13#10#13#10;
              WriteBytes(Pointer(vHeadText)^, Length(vHeadText));
            end;
        end;
      finally
        vRequest.Free;
        vResponse.Free;
        vContext.Free;
      end;
    except
      // a broken connection ends here, never the process
    end;
  finally
    vEnv.Free;
    FChannel.Close;
  end;
end;

{ TRALGatewayServer }

constructor TRALGatewayServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnections := TList.Create;
  FConnLock := TCriticalSection.Create;
  FMaxHeaderSize := 65536;
  FReadTimeout := 30000;
end;

destructor TRALGatewayServer.Destroy;
begin
  Active := False;
  FreeAndNil(FConnections);
  FreeAndNil(FConnLock);
  inherited;
end;

class function TRALGatewayServer.ReasonPhrase(AStatusCode: IntegerRAL): string;
begin
  Result := string(RALHTTPReason(AStatusCode));
  if Result = '' then
    Result := 'Unknown';
end;

class function TRALGatewayServer.SupportsLiveStream: boolean;
begin
  Result := True;
end;

function TRALGatewayServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  if AResponse.EngineContext is TRALGatewayContext then
    Result := TRALGatewayStreamWriter.Create(TRALGatewayContext(AResponse.EngineContext))
  else
    Result := nil;
end;

procedure TRALGatewayServer.ConnectionDone(AConnection: TRALGatewayConnection);
begin
  if FConnLock = nil then
    Exit;
  FConnLock.Acquire;
  try
    FConnections.Remove(AConnection);
  finally
    FConnLock.Release;
  end;
end;

function TRALGatewayServer.ResponseHead(AResponse: TRALResponse; AStatusCode: IntegerRAL;
  ALength: Int64RAL): RawByteString;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vCookies: TStringList;
begin
  Result := StatusLine(AStatusCode) + #13#10;
  if AResponse.ContentType <> '' then
    Result := Result + 'Content-Type: ' + RawByteString(AResponse.ContentType) + #13#10;
  if AResponse.ContentEncoding <> '' then
    Result := Result + 'Content-Encoding: ' + RawByteString(AResponse.ContentEncoding) + #13#10;
  if AResponse.ContentDisposition <> '' then
    Result := Result + 'Content-Disposition: ' + RawByteString(AResponse.ContentDisposition) + #13#10;
  if AResponse.ContentEncription <> '' then
    Result := Result + 'Content-Encription: ' + RawByteString(AResponse.ContentEncription) + #13#10;
  if ALength >= 0 then
    Result := Result + 'Content-Length: ' + RawByteString(IntToStr(ALength)) + #13#10;
  for vInt := 0 to AResponse.Params.Count - 1 do
  begin
    vParam := AResponse.Params.Index[vInt];
    if (vParam.Kind = rpkHEADER) and (vParam.ParamName <> '') then
      Result := Result + RawByteString(vParam.ParamName) + ': ' +
        RawByteString(RALSafeHeaderText(vParam.AsString)) + #13#10;
  end;
  vCookies := TStringList.Create;
  try
    AResponse.GetParamsCookies(vCookies, IncMinute(Now, CookieLife));
    for vInt := 0 to vCookies.Count - 1 do
      Result := Result + 'Set-Cookie: ' + RawByteString(vCookies[vInt]) + #13#10;
  finally
    vCookies.Free;
  end;
  Result := Result + #13#10;
end;

procedure TRALGatewayServer.SetActive(const AValue: boolean);
var
  vInt: IntegerRAL;
begin
  if AValue = Active then
    Exit;
  inherited;
  if AValue then
  begin
    FListener := TRALSocket.Create;
    try
      if IPConfig.IPv4Bind <> '' then
        FListener.Listen(IPConfig.IPv4Bind, Port)
      else
        FListener.Listen('', Port);
    except
      FreeAndNil(FListener);
      inherited SetActive(False);
      raise;
    end;
    FAcceptThread := TRALGatewayAccept.Create(Self);
  end
  else
  begin
    if FAcceptThread <> nil then
    begin
      FAcceptThread.Terminate;
      FAcceptThread.WaitFor;
      FreeAndNil(FAcceptThread);
    end;
    FreeAndNil(FListener);
    { the open connections end; each frees itself, and leaves the list (under
      the lock) before its channel goes, so closing it here is safe }
    FConnLock.Acquire;
    try
      for vInt := 0 to FConnections.Count - 1 do
      begin
        TRALGatewayConnection(FConnections[vInt]).Terminate;
        TRALGatewayConnection(FConnections[vInt]).FChannel.Close;
      end;
    finally
      FConnLock.Release;
    end;
    vInt := 0;
    while (FConnections.Count > 0) and (vInt < 500) do
    begin
      Sleep(10);
      Inc(vInt);
    end;
  end;
end;

end.
