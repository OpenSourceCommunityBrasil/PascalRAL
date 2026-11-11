{ The WebSocket client (RFC 6455) over RAL's own sockets (RALNetPackage), on every
  platform and with no HTTP engine: ws:// and wss://, the certificate rule of
  TRALClient (pins, Verify, OnValidateServerCert), HTTP CONNECT and SOCKS5
  proxies, an authenticator of TRALClient for the handshake, subprotocols,
  permessage-deflate, keep-alive pings and reconnection with backoff. }
unit RALWebSocketClient;

{$I ..\..\core\PascalRAL.inc}

interface

uses
  Classes, SysUtils, SyncObjs,
  RALTypes, RALConsts, RALTools, RALCustomObjects, RALErrorCodes, RALExceptions,
  RALAuthBase, RALClient, RALParams, RALBase64, RALSocket, RALTLS, RALSocketProxy,
  RALWebSocket;

type
  /// TLS settings of TRALWebSocketClient.
  TRALWebSocketSSL = class(TRALSocketSSL);

  /// Proxy settings of TRALWebSocketClient.
  TRALWebSocketProxy = class(TRALSocketProxy);

  TRALWebSocketClient = class;

  /// A whole message arrived; AMessage is freed after the event.
  TRALWebSocketClientOnMessage = procedure(ASender: TRALWebSocketClient;
    AMessage: TRALWebSocketMessage) of object;
  /// The connection ended: the code of the close frame, 1006 when it dropped.
  TRALWebSocketClientOnClose = procedure(ASender: TRALWebSocketClient; ACode: Word;
    const AReason: StringRAL) of object;
  { Something failed: a connection or a reconnection that did not work, a breach
    of the protocol, an event that raised }
  TRALWebSocketClientOnError = procedure(ASender: TRALWebSocketClient;
    AError: Exception) of object;

  { A WebSocket client. Connect opens the connection on the calling thread (the
    handshake included: it raises when refused); a thread of the client then
    reads, answers pings, pings an idle server and, with AutoReconnect, opens
    the connection again after a drop, waiting ReconnectDelay, then twice as
    long each time up to ReconnectMaxDelay. Send, SendBinary, Ping and
    Disconnect may be called from any thread. The events run on the reading
    thread, or on the main thread with SyncEvents }
  TRALWebSocketClient = class(TRALComponent)
  private
    FAuthentication: TRALAuthClient;
    FAutoReconnect: boolean;
    /// The connection; nil while there is none.
    FChannel: TRALChannel;
    FCloseCode: Word;
    FCloseReason: StringRAL;
    /// A close frame was sent on this connection.
    FCloseSent: boolean;
    /// When the close frame was sent.
    FCloseSentTick: Int64RAL;
    /// Frames of the connection.
    FCodec: TRALWSCodec;
    FConnectTimeout: IntegerRAL;
    FDeflate: boolean;
    FDeflateAgreed: boolean;
    FFragmentSize: IntegerRAL;
    FHeaders: TStrings;
    FKeepAliveInterval: IntegerRAL;
    /// When something last arrived.
    FLastReceived: Int64RAL;
    /// Guards FChannel and the writes.
    FLock: TCriticalSection;
    FMaxMessageSize: Int64RAL;
    FOnClose: TRALWebSocketClientOnClose;
    FOnError: TRALWebSocketClientOnError;
    FOnMessage: TRALWebSocketClientOnMessage;
    FOnOpen: TNotifyEvent;
    FOnValidateServerCert: TRALOnValidateCert;
    /// When the ping now unanswered was sent; 0 when none is.
    FPingSentTick: Int64RAL;
    FProtocol: StringRAL;
    FProtocols: StringRAL;
    FProxy: TRALWebSocketProxy;
    FReconnectDelay: IntegerRAL;
    FReconnectMaxDelay: IntegerRAL;
    FSendTimeout: IntegerRAL;
    FSSL: TRALWebSocketSSL;
    FSyncEvents: boolean;
    /// The reading thread.
    FThread: TThread;
    /// TLS context of wss://.
    FTLSContext: TRALTLSContext;
    FURL: StringRAL;
    /// Disconnect was asked: no reconnection.
    FWantClosed: boolean;

    /// A close frame arrived.
    procedure CodecClose(ASender: TRALWSCodec; ACode: Word; const AReason: StringRAL);
    /// A message arrived.
    procedure CodecMessage(ASender: TRALWSCodec; AKind: TRALWSMessageKind; const AData: TBytes);
    /// A ping arrived: answered with a pong.
    procedure CodecPing(ASender: TRALWSCodec; const AData: TBytes);
    /// A pong arrived.
    procedure CodecPong(ASender: TRALWSCodec; const AData: TBytes);
    { The channel is gone: frees it and fires OnClose once. Called by the reading
      thread }
    procedure ConnectionLost;
    { Fires an event on the right thread (SyncEvents); AKind: 0 open, 1 message,
      2 close, 3 error }
    procedure Fire(AKind: IntegerRAL; AMessage: TRALWebSocketMessage; ACode: Word;
      const AReason: StringRAL; AError: Exception);
    function GetConnected: boolean;
    { Opens the socket, the proxy tunnel, TLS and the handshake; leaves the channel
      in FChannel. Raises on any failure }
    procedure OpenConnection;
    /// Reads what arrived and feeds the codec; False when the connection ended.
    function ReadSome: boolean;
    procedure SetAuthentication(const AValue: TRALAuthClient);
    procedure SetHeaders(const AValue: TStrings);
    procedure SetProxy(const AValue: TRALWebSocketProxy);
    procedure SetSSL(const AValue: TRALWebSocketSSL);
    { Pings an idle server and drops one that did not answer; ends a close
      handshake that takes too long }
    procedure Tick;
    /// Writes AData under the lock; False when there is no connection.
    function WriteBytes(const AData: TBytes): boolean;
  protected
    /// Fires OnError with AError, on the main thread with SyncEvents
    procedure DoError(AError: Exception);
    { A whole message arrived, on the reading thread: fires OnMessage (on the
      main thread with SyncEvents). A descendant takes its protocol here }
    procedure DoMessage(AMessage: TRALWebSocketMessage); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Opens the connection to URL and starts the reading thread; raises
      ERALException (or a socket or TLS error) when it cannot. With AutoReconnect
      the thread keeps trying after a later drop }
    procedure Connect;
    { Closes the connection with ACode (the close handshake, up to AWait
      milliseconds) and stops reconnecting }
    procedure Disconnect(ACode: Word = RALWSCloseNormal; const AReason: StringRAL = '';
      AWait: IntegerRAL = 5000);
    /// Sends a ping; False when not connected.
    function Ping: boolean;
    /// Sends AText as a text message; False when not connected.
    function Send(const AText: StringRAL): boolean;
    /// Sends AData as a binary message; False when not connected.
    function SendBinary(const AData: TBytes): boolean;

    /// Code of the close frame that ended the last connection; 1006 when it dropped.
    property CloseCode: Word read FCloseCode;
    /// Reason of the close frame that ended the last connection.
    property CloseReason: StringRAL read FCloseReason;
    /// True while connected.
    property Connected: boolean read GetConnected;
    /// permessage-deflate was agreed on this connection.
    property DeflateAgreed: boolean read FDeflateAgreed;
    /// Subprotocol the server agreed; empty when none.
    property Protocol: StringRAL read FProtocol;
  published
    { Authenticator whose Authorization goes in the handshake (Basic, a Bearer
      token already held: a token is fetched by a TRALClient that shares it) }
    property Authentication: TRALAuthClient read FAuthentication write SetAuthentication;
    /// Opens the connection again after a drop (not after Disconnect).
    property AutoReconnect: boolean read FAutoReconnect write FAutoReconnect default False;
    /// Milliseconds to connect, handshake included.
    property ConnectTimeout: IntegerRAL read FConnectTimeout write FConnectTimeout default 10000;
    /// Offers permessage-deflate; DeflateAgreed says whether the server took it.
    property Deflate: boolean read FDeflate write FDeflate default False;
    /// Largest fragment sent; 0 sends each message in one frame.
    property FragmentSize: IntegerRAL read FFragmentSize write FFragmentSize default 0;
    /// More headers of the handshake, name=value (Origin, Cookie...).
    property Headers: TStrings read FHeaders write SetHeaders;
    { Milliseconds without traffic before a ping; a server that does not answer
      within as long again is dropped. 0 turns it off }
    property KeepAliveInterval: IntegerRAL read FKeepAliveInterval
      write FKeepAliveInterval default 30000;
    /// Largest message accepted, in bytes; 0 is no limit.
    property MaxMessageSize: Int64RAL read FMaxMessageSize write FMaxMessageSize
      default 16777216;
    /// The connection ended.
    property OnClose: TRALWebSocketClientOnClose read FOnClose write FOnClose;
    /// Something failed (see TRALWebSocketClientOnError).
    property OnError: TRALWebSocketClientOnError read FOnError write FOnError;
    /// A message arrived.
    property OnMessage: TRALWebSocketClientOnMessage read FOnMessage write FOnMessage;
    /// The connection opened (also after each reconnection).
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    { Decides whether the server certificate is accepted (wss://); it overrides
      SSL.Pins and the verdict of OpenSSL, as on TRALClient }
    property OnValidateServerCert: TRALOnValidateCert read FOnValidateServerCert
      write FOnValidateServerCert;
    /// Subprotocols offered, separated by commas.
    property Protocols: StringRAL read FProtocols write FProtocols;
    /// Proxy the connection goes through.
    property Proxy: TRALWebSocketProxy read FProxy write SetProxy;
    /// First wait before reconnecting, in milliseconds; doubled each time.
    property ReconnectDelay: IntegerRAL read FReconnectDelay write FReconnectDelay default 1000;
    /// Longest wait before reconnecting, in milliseconds.
    property ReconnectMaxDelay: IntegerRAL read FReconnectMaxDelay
      write FReconnectMaxDelay default 30000;
    /// Milliseconds a send may stall before the connection is dropped.
    property SendTimeout: IntegerRAL read FSendTimeout write FSendTimeout default 10000;
    /// TLS of wss://: CA, client certificate, pins, Verify.
    property SSL: TRALWebSocketSSL read FSSL write SetSSL;
    /// Fires the events on the main thread (TThread.Synchronize).
    property SyncEvents: boolean read FSyncEvents write FSyncEvents default False;
    /// ws://host[:port]/path[?query] or wss://...
    property URL: StringRAL read FURL write FURL;
  end;

implementation

type
  /// The thread of a client: reads, ticks and reconnects.
  TRALWebSocketClientThread = class(TThread)
  private
    /// The client it serves.
    FClient: TRALWebSocketClient;
    /// The thread ended.
    FDone: boolean;
  protected
    procedure Execute; override;
  public
    /// A thread that serves AClient, started.
    constructor Create(AClient: TRALWebSocketClient);
  end;

  /// An event to fire on the main thread
  TRALWebSocketClientEvent = class
  public
    /// Client whose event fires.
    Client: TRALWebSocketClient;
    /// Close code (close).
    Code: Word;
    /// The error (error).
    Error: Exception;
    /// 0 open, 1 message, 2 close, 3 error.
    Kind: IntegerRAL;
    Message: TRALWebSocketMessage;
    /// Close reason (close).
    Reason: StringRAL;

    /// Fires the event.
    procedure Run;
  end;

const
  /// How long the thread waits for data before it looks again, in milliseconds.
  cClientWaitStep = 250;

/// Identity of the calling thread.
function CallerThreadID: TThreadID;
begin
  {$IFDEF FPC}
  Result := GetCurrentThreadId;
  {$ELSE}
  Result := TThread.CurrentThread.ThreadID;
  {$ENDIF}
end;

/// Parts of a ws:// or wss:// URL.
procedure SplitURL(const AURL: StringRAL; out ASecure: boolean; out AHost: StringRAL;
  out APort: IntegerRAL; out APath: StringRAL);
var
  vRest, vHostPort: StringRAL;
  vPos: IntegerRAL;
begin
  vRest := RALTrim(AURL);
  if RALSameName(Copy(vRest, 1, 6), 'wss://') then
  begin
    ASecure := True;
    Delete(vRest, 1, 6);
  end
  else if RALSameName(Copy(vRest, 1, 5), 'ws://') then
  begin
    ASecure := False;
    Delete(vRest, 1, 5);
  end
  else
    raise ERALException.CreateFmt(ecWebSocketBadURL, [string(AURL)]);
  vPos := Pos(StringRAL('/'), vRest);
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
    // [IPv6]:port
    vPos := Pos(StringRAL(']'), vHostPort);
    AHost := Copy(vHostPort, 2, vPos - 2);
    if (vPos < Length(vHostPort)) and (vHostPort[vPos + 1] = ':') then
      APort := StrToIntDef(string(Copy(vHostPort, vPos + 2, MaxInt)), APort);
  end
  else
  begin
    vPos := Pos(StringRAL(':'), vHostPort);
    if vPos > 0 then
    begin
      AHost := Copy(vHostPort, 1, vPos - 1);
      APort := StrToIntDef(string(Copy(vHostPort, vPos + 1, MaxInt)), APort);
    end
    else
      AHost := vHostPort;
  end;
  if AHost = '' then
    raise ERALException.CreateFmt(ecWebSocketBadURL, [string(AURL)]);
end;

/// Value of the header AName in the lines of AHead (from the second on).
function HeadValue(AHead: TStrings; const AName: string): string;
var
  vInt, vPos: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to AHead.Count - 1 do
  begin
    vPos := Pos(':', AHead[vInt]);
    if (vPos > 0) and SameText(Trim(Copy(AHead[vInt], 1, vPos - 1)), AName) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + Trim(Copy(AHead[vInt], vPos + 1, MaxInt));
    end;
  end;
end;

/// True when the comma-separated AList holds AToken (case-insensitive).
function HasToken(const AList, AToken: string): boolean;
var
  vParts: TStringList;
  vInt: IntegerRAL;
begin
  Result := False;
  vParts := TStringList.Create;
  try
    vParts.Delimiter := ',';
    vParts.StrictDelimiter := True;
    vParts.DelimitedText := AList;
    for vInt := 0 to vParts.Count - 1 do
      if SameText(Trim(vParts[vInt]), AToken) then
      begin
        Result := True;
        Exit;
      end;
  finally
    vParts.Free;
  end;
end;

{ TRALWebSocketClientEvent }

procedure TRALWebSocketClientEvent.Run;
var
  vClient: TRALWebSocketClient;
begin
  vClient := Client;
  try
    case Kind of
      0:
        if Assigned(vClient.FOnOpen) then
          vClient.FOnOpen(vClient);
      1:
        if Assigned(vClient.FOnMessage) then
          vClient.FOnMessage(vClient, Message);
      2:
        if Assigned(vClient.FOnClose) then
          vClient.FOnClose(vClient, Code, Reason);
      3:
        if Assigned(vClient.FOnError) then
          vClient.FOnError(vClient, Error);
    end;
  except
    on e: Exception do
      if (Kind <> 3) and Assigned(vClient.FOnError) then
        try
          vClient.FOnError(vClient, e);
        except
          // the error handler itself failed
        end;
  end;
end;

{ TRALWebSocketClientThread }

constructor TRALWebSocketClientThread.Create(AClient: TRALWebSocketClient);
begin
  FClient := AClient;
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TRALWebSocketClientThread.Execute;
var
  vDelay, vWaited: IntegerRAL;
  vSocket: TRALSocket;
  vEvents: Word;
begin
  vDelay := FClient.FReconnectDelay;
  try
    while not Terminated do
    begin
      if FClient.FChannel = nil then
      begin
        if FClient.FWantClosed or not FClient.FAutoReconnect then
          Break;
        // reconnecting: wait, then try; the wait doubles up to the ceiling
        vWaited := 0;
        while (vWaited < vDelay) and not Terminated and not FClient.FWantClosed do
        begin
          Sleep(50);
          Inc(vWaited, 50);
        end;
        if Terminated or FClient.FWantClosed then
          Break;
        try
          FClient.OpenConnection;
          vDelay := FClient.FReconnectDelay;
          FClient.Fire(0, nil, 0, '', nil);
        except
          on e: Exception do
          begin
            FClient.Fire(3, nil, 0, '', e);
            vDelay := vDelay * 2;
            if vDelay > FClient.FReconnectMaxDelay then
              vDelay := FClient.FReconnectMaxDelay;
          end;
        end;
        Continue;
      end;

      vSocket := FClient.FChannel.Socket;
      vEvents := RALPollIn;
      if FClient.FChannel.HasBuffered then
        vEvents := RALPollIn
      else if vSocket <> nil then
        vEvents := vSocket.WaitFor(RALPollIn or FClient.FChannel.WantedEvents, cClientWaitStep);
      if Terminated then
        Break;
      if (vEvents <> 0) and not FClient.ReadSome then
        FClient.ConnectionLost
      else
        FClient.Tick;
    end;
  finally
    if FClient.FChannel <> nil then
      FClient.ConnectionLost;
    FDone := True;
  end;
end;

{ TRALWebSocketClient }

constructor TRALWebSocketClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FHeaders := TStringList.Create;
  FSSL := TRALWebSocketSSL.Create;
  FProxy := TRALWebSocketProxy.Create;
  FConnectTimeout := 10000;
  FKeepAliveInterval := 30000;
  FMaxMessageSize := 16777216;
  FReconnectDelay := 1000;
  FReconnectMaxDelay := 30000;
  FSendTimeout := 10000;
end;

destructor TRALWebSocketClient.Destroy;
begin
  Disconnect(RALWSCloseGoingAway, '', 1000);
  FreeAndNil(FCodec);
  FreeAndNil(FTLSContext);
  FreeAndNil(FProxy);
  FreeAndNil(FSSL);
  FreeAndNil(FHeaders);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TRALWebSocketClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAuthentication) then
    FAuthentication := nil;
end;

procedure TRALWebSocketClient.SetAuthentication(const AValue: TRALAuthClient);
begin
  if FAuthentication = AValue then
    Exit;
  if FAuthentication <> nil then
    FAuthentication.RemoveFreeNotification(Self);
  FAuthentication := AValue;
  if FAuthentication <> nil then
    FAuthentication.FreeNotification(Self);
end;

procedure TRALWebSocketClient.SetHeaders(const AValue: TStrings);
begin
  FHeaders.Assign(AValue);
end;

procedure TRALWebSocketClient.SetProxy(const AValue: TRALWebSocketProxy);
begin
  FProxy.Assign(AValue);
end;

procedure TRALWebSocketClient.SetSSL(const AValue: TRALWebSocketSSL);
begin
  FSSL.Assign(AValue);
end;

function TRALWebSocketClient.GetConnected: boolean;
begin
  FLock.Acquire;
  try
    Result := (FChannel <> nil) and not FCloseSent;
  finally
    FLock.Release;
  end;
end;

procedure TRALWebSocketClient.Fire(AKind: IntegerRAL; AMessage: TRALWebSocketMessage;
  ACode: Word; const AReason: StringRAL; AError: Exception);
var
  vEvent: TRALWebSocketClientEvent;
begin
  vEvent := TRALWebSocketClientEvent.Create;
  try
    vEvent.Client := Self;
    vEvent.Kind := AKind;
    vEvent.Message := AMessage;
    vEvent.Code := ACode;
    vEvent.Reason := AReason;
    vEvent.Error := AError;
    if FSyncEvents and (FThread <> nil) and
       (CallerThreadID = FThread.ThreadID) then
      TThread.Synchronize(FThread, {$IFDEF FPC}@{$ENDIF}vEvent.Run)
    else
      vEvent.Run;
  finally
    vEvent.Free;
  end;
end;

procedure TRALWebSocketClient.OpenConnection;
var
  vSecure, vDeflate: boolean;
  vHost, vPath, vKey, vRequest, vHostLine, vName: StringRAL;
  vPort, vInt, vEnd, vRes: IntegerRAL;
  vDeadline: Int64RAL;
  vSocket: TRALSocket;
  vChannel: TRALChannel;
  vTLS: TRALTLSChannel;
  vCert: TRALCertInfo;
  vParams: TRALParams;
  vVars: TStringList;
  vIn: TBytes;
  vInLen: IntegerRAL;
  vBuf: array[0..4095] of Byte;
  vHead: TStringList;
  vRaw: AnsiString;
  vValue: string;
begin
  SplitURL(FURL, vSecure, vHost, vPort, vPath);
  vDeadline := RALSocketNow + FConnectTimeout;
  vSocket := TRALSocket.Create;
  vChannel := TRALSocketChannel.Create(vSocket);
  try
    if FProxy.Kind <> mpkNoProxy then
    begin
      vSocket.Connect(FProxy.Host, FProxy.Port, FConnectTimeout);
      FProxy.OpenTunnel(vChannel, vHost, vPort, vDeadline - RALSocketNow);
    end
    else
      vSocket.Connect(vHost, vPort, FConnectTimeout);

    if vSecure then
    begin
      FreeAndNil(FTLSContext);
      FTLSContext := TRALTLSContext.Create(mtrClient);
      FTLSContext.SetMinVersion(FSSL.MinVersion);
      FTLSContext.ALPN := FSSL.ALPN;
      FTLSContext.LoadSystemCAs;
      FTLSContext.LoadCAFile(FSSL.CAFile);
      FTLSContext.UseCertificateFiles(FSSL.CertFile, FSSL.KeyFile, FSSL.KeyPassword);
      vTLS := TRALTLSChannel.Create(vChannel, FTLSContext);
      vChannel := vTLS;
      vName := FSSL.ServerName;
      if vName = '' then
        vName := vHost;
      if FSSL.ResumeSession then
        vTLS.SessionKey := RALTLSSessionKey(vName, vHost, vPort, FSSL.CertFile);
      vTLS.Handshake(vName, vDeadline - RALSocketNow);
      vCert := vTLS.PeerCertificate;
      if not RALTLSAcceptCertificate(Self, FSSL, FOnValidateServerCert, vCert,
           vHost, vPort) then
        raise ERALException.CreateFmt(ecWebSocketRefused, [string(vCert.Error)]);
    end;

    // the handshake (RFC 6455 4.1)
    vKey := TRALBase64.Encode(RandomBytes(16));
    vHostLine := vHost;
    if Pos(StringRAL(':'), vHostLine) > 0 then
      vHostLine := '[' + vHostLine + ']';
    if not ((vSecure and (vPort = 443)) or ((not vSecure) and (vPort = 80))) then
      vHostLine := vHostLine + ':' + StringRAL(IntToStr(vPort));
    vRequest := 'GET ' + vPath + ' HTTP/1.1'#13#10 +
                'Host: ' + vHostLine + #13#10 +
                'Upgrade: websocket'#13#10 +
                'Connection: Upgrade'#13#10 +
                'Sec-WebSocket-Key: ' + vKey + #13#10 +
                'Sec-WebSocket-Version: 13'#13#10;
    if RALTrim(FProtocols) <> '' then
      vRequest := vRequest + 'Sec-WebSocket-Protocol: ' + RALSafeHeaderText(FProtocols) + #13#10;
    if FDeflate then
      vRequest := vRequest + 'Sec-WebSocket-Extensions: ' + RALWSDeflateOffer + #13#10;
    if FAuthentication <> nil then
    begin
      vParams := TRALParams.Create;
      vVars := TStringList.Create;
      try
        vVars.Sorted := True;
        vVars.Add('method=GET');
        if vSecure then
          vVars.Add('url=https://' + string(vHostLine + vPath))
        else
          vVars.Add('url=http://' + string(vHostLine + vPath));
        FAuthentication.SetAuthHeader(vVars, vParams);
        vRequest := vRequest + vParams.AssignParamsListText(rpkHEADER, ': ');
        if (vRequest <> '') and (vRequest[Length(vRequest)] <> #10) then
          vRequest := vRequest + #13#10;
      finally
        vVars.Free;
        vParams.Free;
      end;
    end;
    for vInt := 0 to FHeaders.Count - 1 do
      if FHeaders.Names[vInt] <> '' then
        vRequest := vRequest + RALSafeHeaderText(StringRAL(FHeaders.Names[vInt])) + ': ' +
          RALSafeHeaderText(StringRAL(FHeaders.ValueFromIndex[vInt])) + #13#10;
    vRequest := vRequest + #13#10;
    vChannel.WriteAll(Pointer(vRequest)^, Length(vRequest), vDeadline - RALSocketNow);

    // the answer, up to the empty line; what follows it is already WebSocket
    vInLen := 0;
    vEnd := -1;
    repeat
      if RALSocketNow >= vDeadline then
        raise ERALException.CreateFmt(ecWebSocketRefused,
          [Format(emWSTimeout, [FConnectTimeout])]);
      vRes := vChannel.ReadWait(vBuf, SizeOf(vBuf), vDeadline - RALSocketNow);
      if vRes = 0 then
        raise ERALException.CreateFmt(ecWebSocketRefused, [emSocketClosed]);
      if vRes > 0 then
      begin
        SetLength(vIn, vInLen + vRes);
        Move(vBuf[0], vIn[vInLen], vRes);
        Inc(vInLen, vRes);
        for vInt := 0 to vInLen - 4 do
          if (vIn[vInt] = 13) and (vIn[vInt + 1] = 10) and (vIn[vInt + 2] = 13) and
             (vIn[vInt + 3] = 10) then
          begin
            vEnd := vInt;
            Break;
          end;
        if (vEnd < 0) and (vInLen > 16384) then
          raise ERALException.CreateFmt(ecWebSocketRefused, [emWSTooLarge]);
      end;
    until vEnd >= 0;

    SetString(vRaw, PAnsiChar(@vIn[0]), vEnd);
    vHead := TStringList.Create;
    try
      vHead.Text := string(vRaw);
      if (vHead.Count = 0) or (Pos(' 101', vHead[0]) = 0) then
      begin
        if vHead.Count > 0 then
          raise ERALException.CreateFmt(ecWebSocketRefused, [vHead[0]])
        else
          raise ERALException.CreateFmt(ecWebSocketRefused, ['']);
      end;
      if not SameText(HeadValue(vHead, 'Upgrade'), 'websocket') then
        raise ERALException.CreateFmt(ecWebSocketRefused, ['Upgrade']);
      if not HasToken(HeadValue(vHead, 'Connection'), 'Upgrade') then
        raise ERALException.CreateFmt(ecWebSocketRefused, ['Connection']);
      if HeadValue(vHead, 'Sec-WebSocket-Accept') <> string(RALWSAccept(vKey)) then
        raise ERALException.CreateFmt(ecWebSocketRefused, [emWSBadAccept]);
      vValue := HeadValue(vHead, 'Sec-WebSocket-Protocol');
      if (vValue <> '') and not HasToken(string(FProtocols), vValue) then
        raise ERALException.CreateFmt(ecWebSocketRefused, ['Sec-WebSocket-Protocol']);
      FProtocol := StringRAL(vValue);
      vValue := HeadValue(vHead, 'Sec-WebSocket-Extensions');
      vDeflate := False;
      if vValue <> '' then
      begin
        if not FDeflate then
          raise ERALException.CreateFmt(ecWebSocketRefused, ['Sec-WebSocket-Extensions']);
        vDeflate := RALWSCheckDeflateAnswer(StringRAL(vValue));
      end;
    finally
      vHead.Free;
    end;
  except
    vChannel.Free;
    raise;
  end;

  FLock.Acquire;
  try
    FreeAndNil(FCodec);
    FCodec := TRALWSCodec.Create(mwrClient);
    FCodec.MaxMessage := FMaxMessageSize;
    FCodec.FragmentSize := FFragmentSize;
    FCodec.Deflate := vDeflate;
    FCodec.OnMessage := {$IFDEF FPC}@{$ENDIF}CodecMessage;
    FCodec.OnPing := {$IFDEF FPC}@{$ENDIF}CodecPing;
    FCodec.OnPong := {$IFDEF FPC}@{$ENDIF}CodecPong;
    FCodec.OnClose := {$IFDEF FPC}@{$ENDIF}CodecClose;
    FDeflateAgreed := vDeflate;
    FCloseSent := False;
    FCloseCode := 0;
    FCloseReason := '';
    FPingSentTick := 0;
    FLastReceived := RALSocketNow;
    FChannel := vChannel;
  finally
    FLock.Release;
  end;
  // frames that came with the answer
  if vInLen > vEnd + 4 then
    try
      FCodec.Feed(vIn[vEnd + 4], vInLen - vEnd - 4);
    except
      on e: ERALWebSocketError do
      begin
        WriteBytes(FCodec.EncodeClose(e.CloseCode));
        raise;
      end;
    end;
end;

procedure TRALWebSocketClient.Connect;
begin
  if Connected then
    Exit;
  // a thread of an earlier connection that ended
  if (FThread <> nil) and TRALWebSocketClientThread(FThread).FDone then
    FreeAndNil(FThread);
  FWantClosed := False;
  OpenConnection;
  if FThread = nil then
    FThread := TRALWebSocketClientThread.Create(Self);
  Fire(0, nil, 0, '', nil);
end;

procedure TRALWebSocketClient.Disconnect(ACode: Word; const AReason: StringRAL;
  AWait: IntegerRAL);
var
  vDeadline: Int64RAL;
begin
  FWantClosed := True;
  if Connected then
  begin
    FLock.Acquire;
    try
      FCloseSent := True;
      FCloseSentTick := RALSocketNow;
    finally
      FLock.Release;
    end;
    if FCodec <> nil then
      WriteBytes(FCodec.EncodeClose(ACode, AReason));
    // the server answers the close frame and ends the connection
    vDeadline := RALSocketNow + AWait;
    while (FChannel <> nil) and (RALSocketNow < vDeadline) and (FThread <> nil) and
          not TRALWebSocketClientThread(FThread).FDone do
    begin
      if CallerThreadID = MainThreadID then
        CheckSynchronize(10)
      else
        Sleep(10);
    end;
  end;
  if FThread <> nil then
  begin
    FThread.Terminate;
    while not TRALWebSocketClientThread(FThread).FDone do
      if CallerThreadID = MainThreadID then
        CheckSynchronize(10)
      else
        Sleep(10);
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

function TRALWebSocketClient.ReadSome: boolean;
var
  vBuf: array[0..16383] of Byte;
  vRes: IntegerRAL;
begin
  Result := True;
  repeat
    FLock.Acquire;
    try
      if FChannel = nil then
      begin
        Result := False;
        Exit;
      end;
      try
        vRes := FChannel.Read(vBuf, SizeOf(vBuf));
      except
        vRes := 0;
      end;
    finally
      FLock.Release;
    end;
    if vRes = 0 then
    begin
      Result := False;
      Exit;
    end;
    if vRes > 0 then
    begin
      FLastReceived := RALSocketNow;
      FPingSentTick := 0;
      try
        FCodec.Feed(vBuf, vRes);
      except
        on e: ERALWebSocketError do
        begin
          Fire(3, nil, 0, '', e);
          if FCloseCode = 0 then
          begin
            FCloseCode := e.CloseCode;
            FCloseReason := StringRAL(e.Message);
          end;
          WriteBytes(FCodec.EncodeClose(e.CloseCode));
          Result := False;
          Exit;
        end;
      end;
      // the close handshake is over: the server ends the TCP connection
      if FCodec.CloseReceived and FCloseSent then
      begin
        Result := False;
        Exit;
      end;
    end;
  until vRes = RALSocketWouldBlock;
end;

procedure TRALWebSocketClient.ConnectionLost;
var
  vChannel: TRALChannel;
  vCode: Word;
begin
  FLock.Acquire;
  try
    vChannel := FChannel;
    FChannel := nil;
    if FCloseCode = 0 then
      FCloseCode := RALWSCloseAbnormal;
    vCode := FCloseCode;
  finally
    FLock.Release;
  end;
  if vChannel = nil then
    Exit;
  try
    vChannel.Free;
  except
    // a link that broke
  end;
  Fire(2, nil, vCode, FCloseReason, nil);
end;

procedure TRALWebSocketClient.CodecMessage(ASender: TRALWSCodec;
  AKind: TRALWSMessageKind; const AData: TBytes);
var
  vMessage: TRALWebSocketMessage;
begin
  vMessage := TRALWebSocketMessage.Create(AKind, AData);
  try
    DoMessage(vMessage);
  finally
    vMessage.Free;
  end;
end;

procedure TRALWebSocketClient.DoMessage(AMessage: TRALWebSocketMessage);
begin
  Fire(1, AMessage, 0, '', nil);
end;

procedure TRALWebSocketClient.DoError(AError: Exception);
begin
  Fire(3, nil, 0, '', AError);
end;

procedure TRALWebSocketClient.CodecPing(ASender: TRALWSCodec; const AData: TBytes);
begin
  WriteBytes(FCodec.EncodeFrame(RALWSOpPong, PByte(AData), Length(AData)));
end;

procedure TRALWebSocketClient.CodecPong(ASender: TRALWSCodec; const AData: TBytes);
begin
  FPingSentTick := 0;
end;

procedure TRALWebSocketClient.CodecClose(ASender: TRALWSCodec; ACode: Word;
  const AReason: StringRAL);
var
  vEcho: Word;
  vSend: boolean;
begin
  if FCloseCode = 0 then
  begin
    FCloseCode := ACode;
    FCloseReason := AReason;
  end;
  FLock.Acquire;
  try
    vSend := not FCloseSent;
    FCloseSent := True;
    FCloseSentTick := RALSocketNow;
  finally
    FLock.Release;
  end;
  if vSend then
  begin
    vEcho := ACode;
    if vEcho = RALWSCloseNoStatus then
      vEcho := 0;
    WriteBytes(FCodec.EncodeClose(vEcho));
  end;
end;

procedure TRALWebSocketClient.Tick;
var
  vNow: Int64RAL;
begin
  vNow := RALSocketNow;
  if FCloseSent then
  begin
    // the server did not end the connection after the close handshake
    if vNow - FCloseSentTick > 5000 then
    begin
      FLock.Acquire;
      try
        if FChannel <> nil then
          FChannel.Socket.Close;
      finally
        FLock.Release;
      end;
    end;
    Exit;
  end;
  if FKeepAliveInterval <= 0 then
    Exit;
  if (FPingSentTick > 0) and (vNow - FPingSentTick > FKeepAliveInterval) then
  begin
    FLock.Acquire;
    try
      if FChannel <> nil then
        FChannel.Socket.Close;
    finally
      FLock.Release;
    end;
    Exit;
  end;
  if (FPingSentTick = 0) and (vNow - FLastReceived > FKeepAliveInterval) then
    Ping;
end;

function TRALWebSocketClient.WriteBytes(const AData: TBytes): boolean;
begin
  Result := False;
  if Length(AData) = 0 then
    Exit;
  FLock.Acquire;
  try
    if FChannel = nil then
      Exit;
    try
      FChannel.WriteAll(AData[0], Length(AData), FSendTimeout);
      Result := True;
    except
      // a link that broke: the reading thread finds it
      FChannel.Socket.Close;
    end;
  finally
    FLock.Release;
  end;
end;

function TRALWebSocketClient.Send(const AText: StringRAL): boolean;
begin
  Result := Connected and WriteBytes(FCodec.EncodeMessage(wmkText, Pointer(AText),
    Length(AText)));
end;

function TRALWebSocketClient.SendBinary(const AData: TBytes): boolean;
begin
  Result := Connected and WriteBytes(FCodec.EncodeMessage(wmkBinary, PByte(AData),
    Length(AData)));
end;

function TRALWebSocketClient.Ping: boolean;
begin
  Result := Connected and WriteBytes(FCodec.EncodeFrame(RALWSOpPing, nil, 0));
  if Result and (FPingSentTick = 0) then
    FPingSentTick := RALSocketNow;
end;

end.
