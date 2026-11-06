/// FastCGI 1.0 server engine (the protocol written here, the same on Delphi and
/// FPC): nginx (fastcgi_pass) or Apache (mod_proxy_fcgi) hand requests to it
/// over TCP; connections are kept (FCGI_KEEP_CONN) and a streamed response
/// goes out live, record by record.
unit RALFastCGIServer;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs, DateUtils,
  RALTypes, RALConsts, RALTools, RALServer, RALRequest, RALResponse, RALParams,
  RALCustomObjects, RALResponseStream, RALSocket;

const
  /// FastCGI record types (the specification, section 8).
  FCGI_BEGIN_REQUEST = 1;
  FCGI_ABORT_REQUEST = 2;
  FCGI_END_REQUEST = 3;
  FCGI_PARAMS = 4;
  FCGI_STDIN = 5;
  FCGI_STDOUT = 6;
  FCGI_STDERR = 7;
  FCGI_DATA = 8;
  FCGI_GET_VALUES = 9;
  FCGI_GET_VALUES_RESULT = 10;
  FCGI_UNKNOWN_TYPE = 11;
  /// The responder role, the only one served.
  FCGI_RESPONDER = 1;
  /// BEGIN_REQUEST flag: keep the connection after the request.
  FCGI_KEEP_CONN = 1;

type
  TRALFastCGIServer = class;

  { One connection from the web server: records are read, one request at a
    time (nginx and Apache do not multiplex), answered, and the connection is
    kept while the web server asks for it. }
  TRALFastCGIConnection = class(TThread)
  private
    FChannel: TRALSocketChannel;
    FServer: TRALFastCGIServer;
    FWriteLock: TCriticalSection;
  protected
    procedure Execute; override;
  public
    /// A connection over ASocket (owned) for AServer.
    constructor Create(AServer: TRALFastCGIServer; ASocket: TRALSocket);
    destructor Destroy; override;

    /// Writes one record of AType for ARequestId (empty content allowed).
    function WriteRecord(AType: Byte; ARequestId: Word; const AData; ASize: IntegerRAL): boolean;
    /// Writes ASize bytes as as many STDOUT records as needed.
    function WriteStdout(ARequestId: Word; const AData; ASize: IntegerRAL): boolean;
    /// Writes END_REQUEST with the application status.
    function WriteEnd(ARequestId: Word; AAppStatus: Integer): boolean;
  end;

  { The listener: accepts the web server's connections and gives each a
    thread. Port is the TCP port nginx's fastcgi_pass points at. }
  TRALFastCGIServer = class(TRALServer)
  private
    FAcceptThread: TThread;
    FConnections: TList;
    FConnLock: TCriticalSection;
    FListener: TRALSocket;
    FMaxConnections: IntegerRAL;
    FReadTimeout: IntegerRAL;
    procedure ConnectionDone(AConnection: TRALFastCGIConnection);
  protected
    function CreateStreamWriter(ARequest: TRALRequest; AResponse: TRALResponse): TRALStreamWriter; override;
    procedure SetActive(const AValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// The header block of a CGI response: Status line, headers, cookies, blank line.
    function ResponseHead(AResponse: TRALResponse; AStatusCode: IntegerRAL): RawByteString;
    class function SupportsLiveStream: boolean; override;
  published
    /// Most connections from the web server at once; 0 is no limit.
    property MaxConnections: IntegerRAL read FMaxConnections write FMaxConnections default 0;
    /// Milliseconds a connection may stay silent in the middle of a request.
    property ReadTimeout: IntegerRAL read FReadTimeout write FReadTimeout default 30000;
  end;

implementation

type
  { what a streamed answer writes through: the connection and the request id }
  TRALFastCGIContext = class
  public
    Connection: TRALFastCGIConnection;
    RequestId: Word;
    Server: TRALFastCGIServer;
    Ended: boolean;
    HeadSent: boolean;
  end;

  TRALFastCGIStreamWriter = class(TRALStreamWriter)
  private
    FContext: TRALFastCGIContext;
  public
    constructor Create(AContext: TRALFastCGIContext);
    function Alive: boolean; override;
    procedure WriteEnd(AAborted: boolean); override;
    procedure WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL); override;
    function WriteRaw(const ABuffer; ASize: IntegerRAL): boolean; override;
  end;

  TRALFastCGIAccept = class(TThread)
  private
    FServer: TRALFastCGIServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TRALFastCGIServer);
  end;

{ TRALFastCGIStreamWriter }

constructor TRALFastCGIStreamWriter.Create(AContext: TRALFastCGIContext);
begin
  inherited Create;
  FContext := AContext;
  FramesItself := True; // FastCGI records delimit the body; no chunks
end;

function TRALFastCGIStreamWriter.Alive: boolean;
begin
  Result := not FContext.Connection.Terminated;
end;

procedure TRALFastCGIStreamWriter.WriteHead(AResponse: TRALHTTPHeaderInfo; AStatusCode: IntegerRAL);
var
  vHead: RawByteString;
begin
  vHead := FContext.Server.ResponseHead(TRALResponse(AResponse), AStatusCode);
  FContext.HeadSent := True;
  FContext.Connection.WriteStdout(FContext.RequestId, Pointer(vHead)^, Length(vHead));
end;

function TRALFastCGIStreamWriter.WriteRaw(const ABuffer; ASize: IntegerRAL): boolean;
begin
  Result := FContext.Connection.WriteStdout(FContext.RequestId, ABuffer, ASize);
end;

procedure TRALFastCGIStreamWriter.WriteEnd(AAborted: boolean);
begin
  FContext.Ended := True;
  { the empty STDOUT closes the stream; END_REQUEST ends the request }
  FContext.Connection.WriteRecord(FCGI_STDOUT, FContext.RequestId, FContext.RequestId, 0);
  if AAborted then
    FContext.Connection.WriteEnd(FContext.RequestId, 1)
  else
    FContext.Connection.WriteEnd(FContext.RequestId, 0);
end;

{ TRALFastCGIAccept }

constructor TRALFastCGIAccept.Create(AServer: TRALFastCGIServer);
begin
  FServer := AServer;
  inherited Create(False);
end;

procedure TRALFastCGIAccept.Execute;
var
  vSocket: TRALSocket;
  vConn: TRALFastCGIConnection;
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
        vConn := TRALFastCGIConnection.Create(FServer, vSocket);
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

{ TRALFastCGIConnection }

constructor TRALFastCGIConnection.Create(AServer: TRALFastCGIServer; ASocket: TRALSocket);
begin
  FServer := AServer;
  FChannel := TRALSocketChannel.Create(ASocket, True);
  FWriteLock := TCriticalSection.Create;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TRALFastCGIConnection.Destroy;
begin
  FServer.ConnectionDone(Self);
  FreeAndNil(FChannel);
  FreeAndNil(FWriteLock);
  inherited;
end;

function TRALFastCGIConnection.WriteRecord(AType: Byte; ARequestId: Word; const AData;
  ASize: IntegerRAL): boolean;
var
  vHead: array[0..7] of Byte;
  vPad: IntegerRAL;
  vZero: array[0..7] of Byte;
begin
  vPad := (8 - (ASize mod 8)) mod 8;
  vHead[0] := 1; // version
  vHead[1] := AType;
  vHead[2] := Byte(ARequestId shr 8);
  vHead[3] := Byte(ARequestId);
  vHead[4] := Byte(ASize shr 8);
  vHead[5] := Byte(ASize);
  vHead[6] := Byte(vPad);
  vHead[7] := 0;
  FillChar(vZero, SizeOf(vZero), 0);
  FWriteLock.Acquire;
  try
    try
      FChannel.WriteAll(vHead, 8, FServer.ReadTimeout);
      if ASize > 0 then
        FChannel.WriteAll(AData, ASize, FServer.ReadTimeout);
      if vPad > 0 then
        FChannel.WriteAll(vZero, vPad, FServer.ReadTimeout);
      Result := True;
    except
      Result := False;
    end;
  finally
    FWriteLock.Release;
  end;
end;

function TRALFastCGIConnection.WriteStdout(ARequestId: Word; const AData; ASize: IntegerRAL): boolean;
var
  vPtr: PByte;
  vPiece: IntegerRAL;
begin
  Result := True;
  vPtr := @AData;
  while Result and (ASize > 0) do
  begin
    vPiece := ASize;
    if vPiece > 65528 then
      vPiece := 65528;
    Result := WriteRecord(FCGI_STDOUT, ARequestId, vPtr^, vPiece);
    Inc(vPtr, vPiece);
    Dec(ASize, vPiece);
  end;
end;

function TRALFastCGIConnection.WriteEnd(ARequestId: Word; AAppStatus: Integer): boolean;
var
  vBody: array[0..7] of Byte;
begin
  FillChar(vBody, SizeOf(vBody), 0);
  vBody[0] := Byte(AAppStatus shr 24);
  vBody[1] := Byte(AAppStatus shr 16);
  vBody[2] := Byte(AAppStatus shr 8);
  vBody[3] := Byte(AAppStatus);
  vBody[4] := 0; // FCGI_REQUEST_COMPLETE
  Result := WriteRecord(FCGI_END_REQUEST, ARequestId, vBody, 8);
end;

{ reads a name-value length: one byte below 128, else four }
function ReadLength(const AData: RawByteString; var APos: IntegerRAL; out ALen: IntegerRAL): boolean;
begin
  Result := False;
  if APos > Length(AData) then
    Exit;
  if Ord(AData[APos]) < 128 then
  begin
    ALen := Ord(AData[APos]);
    Inc(APos);
  end
  else
  begin
    if APos + 3 > Length(AData) then
      Exit;
    ALen := ((Ord(AData[APos]) and $7F) shl 24) or (Ord(AData[APos + 1]) shl 16) or
      (Ord(AData[APos + 2]) shl 8) or Ord(AData[APos + 3]);
    Inc(APos, 4);
  end;
  Result := True;
end;

procedure ParseParams(const AData: RawByteString; AList: TStrings);
var
  vPos, vNameLen, vValueLen: IntegerRAL;
begin
  vPos := 1;
  while vPos <= Length(AData) do
  begin
    if not ReadLength(AData, vPos, vNameLen) or not ReadLength(AData, vPos, vValueLen) then
      Exit;
    if vPos + vNameLen + vValueLen - 1 > Length(AData) then
      Exit;
    AList.Add(string(Copy(AData, vPos, vNameLen)) + '=' +
      string(Copy(AData, vPos + vNameLen, vValueLen)));
    Inc(vPos, vNameLen + vValueLen);
  end;
end;

procedure TRALFastCGIConnection.Execute;
var
  vHead: array[0..7] of Byte;
  vType: Byte;
  vId, vActiveId: Word;
  vLen, vPad, vInt, vPos: IntegerRAL;
  vContent, vParams, vBody, vScratch: RawByteString;
  vKeep, vKeepAfter, vInRequest, vParamsDone: boolean;
  vEnv: TStringList;
  vRequest: TRALRequest;
  vResponse: TRALResponse;
  vName, vValue, vPath: string;
  vContext: TRALFastCGIContext;
  vStream: TStream;
  vHeadText: RawByteString;
  vAnswer: array[0..7] of Byte;
  vPairs: RawByteString;

  function ReadExact(var ABuffer; ACount: IntegerRAL): boolean;
  begin
    Result := (ACount = 0) or (FChannel.ReadWait(ABuffer, ACount, FServer.ReadTimeout) = ACount);
  end;

  procedure AddPair(const AName, AValue: RawByteString);
  begin
    vPairs := vPairs + AnsiChar(Length(AName)) + AnsiChar(Length(AValue)) + AName + AValue;
  end;

begin
  vEnv := TStringList.Create;
  vKeep := True;
  vKeepAfter := True;
  vInRequest := False;
  vActiveId := 0;
  try
    while vKeep and not Terminated do
    begin
      if not ReadExact(vHead, 8) then
        Break;
      vType := vHead[1];
      vId := (vHead[2] shl 8) or vHead[3];
      vLen := (vHead[4] shl 8) or vHead[5];
      vPad := vHead[6];
      SetLength(vContent, vLen);
      if (vLen > 0) and not ReadExact(vContent[1], vLen) then
        Break;
      if vPad > 0 then
      begin
        SetLength(vScratch, vPad);
        if not ReadExact(vScratch[1], vPad) then
          Break;
      end;

      case vType of
        FCGI_GET_VALUES:
        begin
          vPairs := '';
          AddPair('FCGI_MPXS_CONNS', '0');
          AddPair('FCGI_MAX_CONNS', RawByteString(IntToStr(FServer.MaxConnections)));
          AddPair('FCGI_MAX_REQS', RawByteString(IntToStr(FServer.MaxConnections)));
          WriteRecord(FCGI_GET_VALUES_RESULT, 0, Pointer(vPairs)^, Length(vPairs));
        end;
        FCGI_BEGIN_REQUEST:
        begin
          if (vLen < 8) or (((Ord(vContent[1]) shl 8) or Ord(vContent[2])) <> FCGI_RESPONDER) then
          begin
            { another role: FCGI_UNKNOWN_ROLE }
            FillChar(vAnswer, SizeOf(vAnswer), 0);
            vAnswer[4] := 3;
            WriteRecord(FCGI_END_REQUEST, vId, vAnswer, 8);
            Continue;
          end;
          { without FCGI_KEEP_CONN the connection closes when this request ends }
          vKeepAfter := (Ord(vContent[3]) and FCGI_KEEP_CONN) <> 0;
          vActiveId := vId;
          vInRequest := True;
          vParamsDone := False;
          vParams := '';
          vBody := '';
          vEnv.Clear;
        end;
        FCGI_ABORT_REQUEST:
        begin
          if vInRequest and (vId = vActiveId) then
          begin
            vInRequest := False;
            WriteEnd(vId, 0);
            vKeep := vKeepAfter;
          end;
        end;
        FCGI_PARAMS:
          if vInRequest and (vId = vActiveId) then
          begin
            if vLen = 0 then
            begin
              vParamsDone := True;
              ParseParams(vParams, vEnv);
            end
            else
              vParams := vParams + vContent;
          end;
        FCGI_STDIN:
          if vInRequest and (vId = vActiveId) then
          begin
            if vLen > 0 then
              vBody := vBody + vContent
            else
            begin
              { the body is whole: the request runs }
              vInRequest := False;
              vRequest := FServer.CreateRequest;
              vResponse := FServer.CreateResponse;
              vContext := TRALFastCGIContext.Create;
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
                vRequest.ContentSize := StrToInt64Def(vEnv.Values['CONTENT_LENGTH'], 0);
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

                FServer.ValidateRequest(vRequest, vResponse);
                if vResponse.StatusCode < HTTP_BadRequest then
                  vRequest.SetWireBody(vBody);
                vContext.Connection := Self;
                vContext.RequestId := vActiveId;
                vContext.Server := FServer;
                vResponse.EngineContext := vContext;
                try
                FServer.ProcessCommands(vRequest, vResponse);
                if not FServer.FinishStream(vRequest, vResponse) then
                begin
                  vStream := vResponse.TakeWireStream;
                  try
                    vRequest.Finish;
                    vHeadText := FServer.ResponseHead(vResponse, vResponse.StatusCode);
                    WriteStdout(vActiveId, Pointer(vHeadText)^, Length(vHeadText));
                    if (vStream <> nil) and (vRequest.Method <> amHEAD) then
                    begin
                      vStream.Position := 0;
                      SetLength(vScratch, 65528);
                      repeat
                        vPos := vStream.Read(vScratch[1], 65528);
                        if vPos > 0 then
                          WriteRecord(FCGI_STDOUT, vActiveId, vScratch[1], vPos);
                      until vPos <= 0;
                    end;
                  finally
                    vStream.Free;
                  end;
                  WriteRecord(FCGI_STDOUT, vActiveId, vHead, 0);
                  WriteEnd(vActiveId, 0);
                end
                else if not vContext.Ended then
                begin
                  WriteRecord(FCGI_STDOUT, vActiveId, vHead, 0);
                  WriteEnd(vActiveId, 0);
                end;
                except
                  on E: Exception do
                  begin
                    { nothing sent yet: a 500; else the request ends cut }
                    if not vContext.HeadSent then
                    begin
                      vHeadText := 'Status: 500'#13#10'Content-Type: text/plain'#13#10#13#10;
                      WriteStdout(vActiveId, Pointer(vHeadText)^, Length(vHeadText));
                    end;
                    if not vContext.Ended then
                    begin
                      WriteRecord(FCGI_STDOUT, vActiveId, vHead, 0);
                      WriteEnd(vActiveId, 1);
                    end;
                  end;
                end;
              finally
                vRequest.Free;
                vResponse.Free;
                vContext.Free;
              end;
              vBody := '';
              vKeep := vKeepAfter;
            end;
          end;
      else
        if vType > FCGI_UNKNOWN_TYPE then
        begin
          FillChar(vAnswer, SizeOf(vAnswer), 0);
          vAnswer[0] := vType;
          WriteRecord(FCGI_UNKNOWN_TYPE, 0, vAnswer, 8);
        end;
      end;
    end;
  finally
    vEnv.Free;
    FChannel.Close;
  end;
end;

{ TRALFastCGIServer }

constructor TRALFastCGIServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnections := TList.Create;
  FConnLock := TCriticalSection.Create;
  FReadTimeout := 30000;
  Port := 9000;
  SetEngine('FastCGI');
end;

destructor TRALFastCGIServer.Destroy;
begin
  Active := False;
  FreeAndNil(FConnections);
  FreeAndNil(FConnLock);
  inherited;
end;

class function TRALFastCGIServer.SupportsLiveStream: boolean;
begin
  Result := True;
end;

function TRALFastCGIServer.CreateStreamWriter(ARequest: TRALRequest;
  AResponse: TRALResponse): TRALStreamWriter;
begin
  if AResponse.EngineContext is TRALFastCGIContext then
    Result := TRALFastCGIStreamWriter.Create(TRALFastCGIContext(AResponse.EngineContext))
  else
    Result := nil;
end;

procedure TRALFastCGIServer.ConnectionDone(AConnection: TRALFastCGIConnection);
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

function TRALFastCGIServer.ResponseHead(AResponse: TRALResponse; AStatusCode: IntegerRAL): RawByteString;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
  vCookies: TStringList;
begin
  Result := RawByteString('Status: ' + IntToStr(AStatusCode)) + #13#10;
  if AResponse.ContentType <> '' then
    Result := Result + 'Content-Type: ' + RawByteString(AResponse.ContentType) + #13#10;
  if AResponse.ContentEncoding <> '' then
    Result := Result + 'Content-Encoding: ' + RawByteString(AResponse.ContentEncoding) + #13#10;
  if AResponse.ContentDisposition <> '' then
    Result := Result + 'Content-Disposition: ' + RawByteString(AResponse.ContentDisposition) + #13#10;
  if AResponse.ContentEncription <> '' then
    Result := Result + 'Content-Encription: ' + RawByteString(AResponse.ContentEncription) + #13#10;
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

procedure TRALFastCGIServer.SetActive(const AValue: boolean);
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
    FAcceptThread := TRALFastCGIAccept.Create(Self);
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
        TRALFastCGIConnection(FConnections[vInt]).Terminate;
        TRALFastCGIConnection(FConnections[vInt]).FChannel.Close;
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
