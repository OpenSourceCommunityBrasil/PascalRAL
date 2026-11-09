{ WebSocket (RFC 6455) as a channel over another one (RALNetPackage), client
  and server: the handshake with an optional subprotocol (Protocol; MQTT
  asks for "mqtt"), binary frames, the bytes as one stream that may cross
  frame boundaries either way, ping/pong and closing with a status code. }
unit RALWebSocket;

{$I ..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$IFDEF DELPHIXE3UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALSocket;

const
  /// Normal closure.
  RALWSCloseNormal = 1000;
  /// Going away (the server stops).
  RALWSCloseGoingAway = 1001;
  /// Protocol error.
  RALWSCloseProtocolError = 1002;
  /// Data of a type the endpoint cannot accept (a text frame).
  RALWSCloseUnsupported = 1003;
  /// Message too big.
  RALWSCloseTooBig = 1009;

type
  /// Raised when the handshake or a frame breaks RFC 6455.
  ERALWebSocketError = class(ERALSocketError)
  private
    FCloseCode: Word;
  public
    /// AMessage, with ACloseCode (RALWSClose*; 0 for a failed handshake).
    constructor CreateClose(ACloseCode: Word; const AMessage: string);

    /// The close code the failure calls for; 0 for a failed handshake.
    property CloseCode: Word read FCloseCode;
  end;

  /// Which side of the handshake a WebSocket channel plays.
  TRALWSRole = (mwrClient, mwrServer);

  { WebSocket over another channel (a socket or TLS). Read hands out the bytes
    of binary frames as one stream; Write sends each call as one binary frame
    (masked on the client side, as RFC 6455 5.3 asks) }
  TRALWebSocketChannel = class(TRALLayerChannel)
  private
    /// Whether the close handshake ended.
    FClosed: boolean;
    /// Whether a Close frame was sent.
    FCloseSent: boolean;
    FHandshakeDone: boolean;
    /// Bytes read and not yet parsed.
    FIn: TBytes;
    /// Start of the unparsed bytes in FIn.
    FInHead: IntegerRAL;
    /// End of the bytes in FIn.
    FInTail: IntegerRAL;
    FMaxFrame: Int64RAL;
    /// Frames queued and not yet written.
    FOut: TBytes;
    /// Start of the unwritten bytes in FOut.
    FOutHead: IntegerRAL;
    /// End of the bytes in FOut.
    FOutTail: IntegerRAL;
    FPath: StringRAL;
    /// The payload of the message being read.
    FPayload: TBytes;
    /// Start of the unread payload.
    FPayloadHead: IntegerRAL;
    /// End of the payload.
    FPayloadTail: IntegerRAL;
    /// The subprotocols the peer offered.
    FPeerProtocols: StringRAL;
    FProtocol: StringRAL;
    FRequestHeaders: TStringList;
    /// Client (masks frames) or server.
    FRole: TRALWSRole;

    /// Appends ACount bytes read to FIn.
    procedure AppendIn(const ABuffer; ACount: IntegerRAL);
    /// Appends ACount payload bytes.
    procedure AppendPayload(AData: PByte; ACount: IntegerRAL);
    /// Reads what the inner stream has; returns the bytes read.
    function Fill: IntegerRAL;
    /// Writes the queued frames to the inner stream.
    procedure FlushOut;
    /// Parses the frames in FIn; True when payload became available.
    function ParseFrames: boolean;
    /// Queues a frame of AOpcode with ACount bytes of AData.
    procedure QueueFrame(AOpcode: Byte; AData: PByte; ACount: IntegerRAL);
    /// Reads the client handshake; True once it is complete and answered.
    function ServerHandshakeParse: boolean;
  public
    /// WebSocket over AInner in ARole.
    constructor Create(AInner: TRALChannel; ARole: TRALWSRole;
      AOwnsInner: boolean = True);
    destructor Destroy; override;

    procedure Close; override;
    function Flush: boolean; override;
    { Client handshake: GET APath, asking for Protocol when set, to AHost (sent in
      Host; with APort when it is not the default), within ATimeout ms.
      AHeaders are added to the request (name: value lines) }
    procedure Handshake(const AHost: StringRAL; APort: IntegerRAL; const APath: StringRAL;
      ASecure: boolean; ATimeout: IntegerRAL; AHeaders: TStrings = nil);
    function HasBuffered: boolean; override;
    /// True while frames wait to be written.
    function HasPendingWrite: boolean;
    function Read(var ABuffer; ACount: IntegerRAL): IntegerRAL; override;
    /// Sends a close frame with ACode, once.
    procedure SendClose(ACode: Word);
    { One step of the server handshake; True when it finished (the 101 was
      queued). Raises when the request is not a WebSocket one for APath offering
      Protocol (when set); a 400 or 404 is sent first }
    function ServerHandshakeStep(const APath: StringRAL): boolean;
    function WantedEvents: Word; override;
    function Write(const ABuffer; ACount: IntegerRAL): IntegerRAL; override;

    /// The handshake is over.
    property HandshakeDone: boolean read FHandshakeDone;
    /// Largest frame accepted, in bytes; 0 is no limit.
    property MaxFrame: Int64RAL read FMaxFrame write FMaxFrame;
    /// Server side: path the client asked for.
    property Path: StringRAL read FPath;
    { Subprotocol asked for (client) or required of the client (server);
      empty: none asked, none required }
    property Protocol: StringRAL read FProtocol write FProtocol;
    /// Server side: headers of the client's request (name=value).
    property RequestHeaders: TStringList read FRequestHeaders;
  end;

/// Sec-WebSocket-Accept for AKey (RFC 6455 4.2.2).
function RALWSAccept(const AKey: StringRAL): StringRAL;

implementation

uses
  RALSHA1, RALBase64, RALTools;

const
  /// The GUID of the Sec-WebSocket-Accept computation (RFC 6455).
  cGUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  /// Longest handshake head, in bytes.
  cMaxHandshake = 16384;

function RALWSAccept(const AKey: StringRAL): StringRAL;
var
  vSHA: TRALSHA1;
  vText: StringRAL;
begin
  vText := RALTrim(AKey) + cGUID;
  vSHA := TRALSHA1.Create;
  try
    vSHA.HashBegin;
    vSHA.HashUpdate(Pointer(vText), Length(vText));
    Result := TRALBase64.Encode(vSHA.HashEnd);
  finally
    vSHA.Free;
  end;
end;

/// The value of header AName (case-insensitive) in AHead, '' when absent.
function HeaderValue(AHead: TStrings; const AName: string): string;
var
  vInt, vPos: IntegerRAL;
  vLine: string;
begin
  Result := '';
  for vInt := 1 to AHead.Count - 1 do
  begin
    vLine := AHead[vInt];
    vPos := Pos(':', vLine);
    if (vPos > 0) and SameText(Trim(Copy(vLine, 1, vPos - 1)), AName) then
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + Trim(Copy(vLine, vPos + 1, MaxInt));
    end;
  end;
end;

/// True when the comma-separated list AList holds AToken (case-insensitive).
function ListHas(const AList, AToken: string): boolean;
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

/// Raises ERALWebSocketError for a failed handshake.
procedure HandshakeError(const AReason: string);
begin
  raise ERALWebSocketError.CreateClose(0, Format(emWSHandshake, [AReason]));
end;

/// Raises ERALWebSocketError for a malformed frame (close code 1002).
procedure FrameError(const AReason: string);
begin
  raise ERALWebSocketError.CreateClose(RALWSCloseProtocolError,
    Format(emWSBadFrame, [AReason]));
end;

{ ERALWebSocketError }

constructor ERALWebSocketError.CreateClose(ACloseCode: Word; const AMessage: string);
begin
  inherited Create(AMessage);
  FCloseCode := ACloseCode;
end;

{ TRALWebSocketChannel }

constructor TRALWebSocketChannel.Create(AInner: TRALChannel;
  ARole: TRALWSRole; AOwnsInner: boolean);
begin
  inherited Create(AInner, AOwnsInner);
  FRole := ARole;
  FRequestHeaders := TStringList.Create;
end;

destructor TRALWebSocketChannel.Destroy;
begin
  FreeAndNil(FRequestHeaders);
  inherited Destroy;
end;

procedure TRALWebSocketChannel.AppendIn(const ABuffer; ACount: IntegerRAL);
begin
  if FInHead > 0 then
  begin
    if FInTail > FInHead then
      Move(FIn[FInHead], FIn[0], FInTail - FInHead);
    Dec(FInTail, FInHead);
    FInHead := 0;
  end;
  if FInTail + ACount > Length(FIn) then
    SetLength(FIn, (FInTail + ACount) * 2 + 1024);
  Move(ABuffer, FIn[FInTail], ACount);
  Inc(FInTail, ACount);
end;

procedure TRALWebSocketChannel.AppendPayload(AData: PByte; ACount: IntegerRAL);
begin
  if ACount <= 0 then
    Exit;
  if FPayloadHead > 0 then
  begin
    if FPayloadTail > FPayloadHead then
      Move(FPayload[FPayloadHead], FPayload[0], FPayloadTail - FPayloadHead);
    Dec(FPayloadTail, FPayloadHead);
    FPayloadHead := 0;
  end;
  if FPayloadTail + ACount > Length(FPayload) then
    SetLength(FPayload, (FPayloadTail + ACount) * 2 + 1024);
  Move(AData^, FPayload[FPayloadTail], ACount);
  Inc(FPayloadTail, ACount);
end;

{ reads what the channel below has; the bytes read, 0 when it closed,
  RALSocketWouldBlock when there was nothing }
function TRALWebSocketChannel.Fill: IntegerRAL;
var
  vBuf: array[0..16383] of Byte;
begin
  Result := Inner.Read(vBuf, SizeOf(vBuf));
  if Result > 0 then
    AppendIn(vBuf, Result);
end;

procedure TRALWebSocketChannel.QueueFrame(AOpcode: Byte; AData: PByte;
  ACount: IntegerRAL);
var
  vHeader: array[0..13] of Byte;
  vLen, vInt, vNeed: IntegerRAL;
  vMask: TBytes;
begin
  vHeader[0] := $80 or AOpcode; // FIN: nothing here sends fragmented frames
  vLen := 2;
  if ACount < 126 then
    vHeader[1] := ACount
  else if ACount < 65536 then
  begin
    vHeader[1] := 126;
    vHeader[2] := Byte(ACount shr 8);
    vHeader[3] := Byte(ACount);
    vLen := 4;
  end
  else
  begin
    vHeader[1] := 127;
    for vInt := 0 to 3 do
      vHeader[2 + vInt] := 0;
    vHeader[6] := Byte(ACount shr 24);
    vHeader[7] := Byte(ACount shr 16);
    vHeader[8] := Byte(ACount shr 8);
    vHeader[9] := Byte(ACount);
    vLen := 10;
  end;
  if FRole = mwrClient then
  begin
    vHeader[1] := vHeader[1] or $80;
    vMask := RandomBytes(4);
    Move(vMask[0], vHeader[vLen], 4);
    Inc(vLen, 4);
  end;

  if FOutHead > 0 then
  begin
    if FOutTail > FOutHead then
      Move(FOut[FOutHead], FOut[0], FOutTail - FOutHead);
    Dec(FOutTail, FOutHead);
    FOutHead := 0;
  end;
  vNeed := FOutTail + vLen + ACount;
  if vNeed > Length(FOut) then
    SetLength(FOut, vNeed * 2 + 256);
  Move(vHeader[0], FOut[FOutTail], vLen);
  Inc(FOutTail, vLen);
  if ACount > 0 then
  begin
    Move(AData^, FOut[FOutTail], ACount);
    if FRole = mwrClient then
      for vInt := 0 to ACount - 1 do
        FOut[FOutTail + vInt] := FOut[FOutTail + vInt] xor vMask[vInt and 3];
    Inc(FOutTail, ACount);
  end;
end;

procedure TRALWebSocketChannel.FlushOut;
var
  vRes: IntegerRAL;
begin
  while FOutHead < FOutTail do
  begin
    vRes := Inner.Write(FOut[FOutHead], FOutTail - FOutHead);
    if vRes <= 0 then
      Exit;
    Inc(FOutHead, vRes);
  end;
  FOutHead := 0;
  FOutTail := 0;
end;

function TRALWebSocketChannel.Flush: boolean;
begin
  FlushOut;
  Result := (FOutHead >= FOutTail) and Inner.Flush;
end;

function TRALWebSocketChannel.HasPendingWrite: boolean;
begin
  Result := FOutHead < FOutTail;
end;

{ takes every whole frame out of FIn; False when a close frame ended it }
function TRALWebSocketChannel.ParseFrames: boolean;
var
  vAvail, vHeader, vInt: IntegerRAL;
  vFirst, vSecond, vOpcode: Byte;
  vLen: Int64RAL;
  vMasked: boolean;
  vMask: array[0..3] of Byte;
  vData: PByte;
  vCode: Word;
begin
  Result := True;
  while True do
  begin
    vAvail := FInTail - FInHead;
    if vAvail < 2 then
      Exit;
    vFirst := FIn[FInHead];
    vSecond := FIn[FInHead + 1];
    if (vFirst and $70) <> 0 then // RSV bits: no extension was agreed
      FrameError('RSV');
    vOpcode := vFirst and $0F;
    vMasked := (vSecond and $80) <> 0;
    // a server takes only masked frames, a client only unmasked ones (5.1)
    if vMasked <> (FRole = mwrServer) then
      FrameError('mask');
    vLen := vSecond and $7F;
    vHeader := 2;
    if vLen = 126 then
    begin
      if vAvail < 4 then
        Exit;
      vLen := (Int64RAL(FIn[FInHead + 2]) shl 8) or FIn[FInHead + 3];
      vHeader := 4;
    end
    else if vLen = 127 then
    begin
      if vAvail < 10 then
        Exit;
      vLen := 0;
      for vInt := 2 to 9 do
        vLen := (vLen shl 8) or FIn[FInHead + vInt];
      vHeader := 10;
      if vLen < 0 then
        FrameError('length');
    end;
    if (FMaxFrame > 0) and (vLen > FMaxFrame) then
      raise ERALWebSocketError.CreateClose(RALWSCloseTooBig,
        Format(emWSBadFrame, ['size']));
    if vLen > MaxInt - 64 then
      FrameError('length');
    if vMasked then
    begin
      if vAvail < vHeader + 4 then
        Exit;
      Move(FIn[FInHead + vHeader], vMask[0], 4);
      Inc(vHeader, 4);
    end;
    if vAvail < vHeader + vLen then
      Exit;

    vData := @FIn[FInHead + vHeader];
    if vMasked then
      for vInt := 0 to IntegerRAL(vLen) - 1 do
        vData[vInt] := vData[vInt] xor vMask[vInt and 3];
    Inc(FInHead, vHeader + IntegerRAL(vLen));

    case vOpcode of
      0, 2: // continuation, binary: the bytes, frames or not [MQTT-6.0.0-2]
        AppendPayload(vData, IntegerRAL(vLen));
      1: // text [MQTT-6.0.0-1]
      begin
        SendClose(RALWSCloseUnsupported);
        raise ERALWebSocketError.CreateClose(RALWSCloseUnsupported, emWSTextFrame);
      end;
      8: // close: answered with the same code, then the stream ends
      begin
        vCode := RALWSCloseNormal;
        if vLen >= 2 then
          vCode := (Word(vData[0]) shl 8) or vData[1];
        SendClose(vCode);
        FClosed := True;
        Result := False;
        Exit;
      end;
      9: // ping, answered with the same data
      begin
        if (vLen > 125) or ((vFirst and $80) = 0) then
          FrameError('ping');
        QueueFrame(10, vData, IntegerRAL(vLen));
        FlushOut;
      end;
      10: ; // pong
    else
      FrameError('opcode');
    end;
  end;
end;

procedure TRALWebSocketChannel.Handshake(const AHost: StringRAL; APort: IntegerRAL;
  const APath: StringRAL; ASecure: boolean; ATimeout: IntegerRAL; AHeaders: TStrings);
var
  vKey, vRequest, vHostLine: StringRAL;
  vDeadline, vNow: Int64RAL;
  vRes, vEnd, vInt: IntegerRAL;
  vHead: TStringList;
  vText: string;
  vRaw: AnsiString;
begin
  vKey := TRALBase64.Encode(RandomBytes(16));
  vHostLine := AHost;
  if Pos(StringRAL(':'), vHostLine) > 0 then
    vHostLine := '[' + vHostLine + ']'; // an IPv6 literal
  if not ((ASecure and (APort = 443)) or ((not ASecure) and (APort = 80))) then
    vHostLine := vHostLine + ':' + StringRAL(IntToStr(APort));
  vRequest := 'GET ' + APath + ' HTTP/1.1'#13#10 +
              'Host: ' + vHostLine + #13#10 +
              'Upgrade: websocket'#13#10 +
              'Connection: Upgrade'#13#10 +
              'Sec-WebSocket-Key: ' + vKey + #13#10 +
              'Sec-WebSocket-Version: 13'#13#10;
  if FProtocol <> '' then
    // [MQTT-6.0.0-3]
    vRequest := vRequest + 'Sec-WebSocket-Protocol: ' + FProtocol + #13#10;
  if AHeaders <> nil then
    for vInt := 0 to AHeaders.Count - 1 do
      if Trim(AHeaders[vInt]) <> '' then
        vRequest := vRequest + RALSafeHeaderText(StringRAL(AHeaders[vInt])) + #13#10;
  vRequest := vRequest + #13#10;
  Inner.WriteAll(Pointer(vRequest)^, Length(vRequest), ATimeout);

  // the answer, up to the empty line; what follows it is already WebSocket
  vDeadline := RALSocketNow + ATimeout;
  vEnd := -1;
  repeat
    for vInt := FInHead to FInTail - 4 do
      if (FIn[vInt] = 13) and (FIn[vInt + 1] = 10) and (FIn[vInt + 2] = 13) and
         (FIn[vInt + 3] = 10) then
      begin
        vEnd := vInt;
        Break;
      end;
    if vEnd >= 0 then
      Break;
    if FInTail - FInHead > cMaxHandshake then
      HandshakeError(emWSTooLarge);
    vRes := Fill;
    if vRes = 0 then
      HandshakeError(emSocketClosed);
    if vRes = RALSocketWouldBlock then
    begin
      vNow := RALSocketNow;
      if vNow >= vDeadline then
        HandshakeError(Format(emWSTimeout, [ATimeout]));
      Socket.WaitFor(Inner.WantedEvents, vDeadline - vNow);
    end;
  until False;

  SetString(vRaw, PAnsiChar(@FIn[FInHead]), vEnd - FInHead);
  vText := string(vRaw);
  FInHead := vEnd + 4;
  vHead := TStringList.Create;
  try
    vHead.Text := vText;
    if (vHead.Count = 0) or (Pos(' 101', vHead[0]) = 0) then
    begin
      if vHead.Count > 0 then
        HandshakeError(Format(emWSStatus, [vHead[0]]))
      else
        HandshakeError(Format(emWSStatus, ['']));
    end;
    if not SameText(HeaderValue(vHead, 'Upgrade'), 'websocket') then
      HandshakeError('Upgrade');
    if not ListHas(HeaderValue(vHead, 'Connection'), 'Upgrade') then
      HandshakeError('Connection');
    if HeaderValue(vHead, 'Sec-WebSocket-Accept') <> string(RALWSAccept(vKey)) then
      HandshakeError(emWSBadAccept);
    if (FProtocol <> '') and
       (HeaderValue(vHead, 'Sec-WebSocket-Protocol') <> string(FProtocol)) then
      // [MQTT-6.0.0-4]
      HandshakeError(emWSNoProtocol);
  finally
    vHead.Free;
  end;
  FHandshakeDone := True;
  // frames that came with the answer
  ParseFrames;
end;

{ parses the client request in FIn once it is whole; False while it is not }
function TRALWebSocketChannel.ServerHandshakeParse: boolean;
var
  vInt, vEnd, vPos: IntegerRAL;
  vText, vLine: string;
  vRaw: AnsiString;
  vHead: TStringList;
begin
  Result := False;
  vEnd := -1;
  for vInt := FInHead to FInTail - 4 do
    if (FIn[vInt] = 13) and (FIn[vInt + 1] = 10) and (FIn[vInt + 2] = 13) and
       (FIn[vInt + 3] = 10) then
    begin
      vEnd := vInt;
      Break;
    end;
  if vEnd < 0 then
  begin
    if FInTail - FInHead > cMaxHandshake then
      HandshakeError(emWSTooLarge);
    Exit;
  end;
  SetString(vRaw, PAnsiChar(@FIn[FInHead]), vEnd - FInHead);
  vText := string(vRaw);
  FInHead := vEnd + 4;
  vHead := TStringList.Create;
  try
    vHead.Text := vText;
    FRequestHeaders.Clear;
    for vInt := 1 to vHead.Count - 1 do
    begin
      vLine := vHead[vInt];
      vPos := Pos(':', vLine);
      if vPos > 0 then
        FRequestHeaders.Add(Trim(Copy(vLine, 1, vPos - 1)) + '=' + Trim(Copy(vLine, vPos + 1, MaxInt)));
    end;
    // GET <path> HTTP/1.1
    vLine := '';
    if vHead.Count > 0 then
      vLine := vHead[0];
    if Copy(vLine, 1, 4) <> 'GET ' then
      HandshakeError('GET');
    vLine := Copy(vLine, 5, MaxInt);
    vPos := Pos(' ', vLine);
    if vPos > 0 then
      vLine := Copy(vLine, 1, vPos - 1);
    vPos := Pos('?', vLine);
    if vPos > 0 then
      vLine := Copy(vLine, 1, vPos - 1);
    FPath := StringRAL(vLine);
    FPeerProtocols := StringRAL(HeaderValue(vHead, 'Sec-WebSocket-Protocol'));
    if (not SameText(HeaderValue(vHead, 'Upgrade'), 'websocket')) or
       (not ListHas(HeaderValue(vHead, 'Connection'), 'Upgrade')) or
       (HeaderValue(vHead, 'Sec-WebSocket-Version') <> '13') or
       (HeaderValue(vHead, 'Sec-WebSocket-Key') = '') then
    begin
      vText := 'HTTP/1.1 400 Bad Request'#13#10'Sec-WebSocket-Version: 13'#13#10 +
               'Content-Length: 0'#13#10'Connection: close'#13#10#13#10;
      vRaw := AnsiString(vText);
      Inner.Write(Pointer(vRaw)^, Length(vRaw));
      HandshakeError('Upgrade');
    end;
    vText := HeaderValue(vHead, 'Sec-WebSocket-Key');
  finally
    vHead.Free;
  end;
  vLine := 'HTTP/1.1 101 Switching Protocols'#13#10 +
           'Upgrade: websocket'#13#10 +
           'Connection: Upgrade'#13#10 +
           'Sec-WebSocket-Accept: ' + string(RALWSAccept(StringRAL(vText))) + #13#10;
  if FProtocol <> '' then
    vLine := vLine + 'Sec-WebSocket-Protocol: ' + string(FProtocol) + #13#10;
  vLine := vLine + #13#10;
  vRaw := AnsiString(vLine);
  Result := True;
  // the 101 goes through the frame queue, so a full socket does not lose it
  if FOutHead > 0 then
  begin
    Move(FOut[FOutHead], FOut[0], FOutTail - FOutHead);
    Dec(FOutTail, FOutHead);
    FOutHead := 0;
  end;
  if FOutTail + Length(vRaw) > Length(FOut) then
    SetLength(FOut, FOutTail + Length(vRaw) + 256);
  Move(Pointer(vRaw)^, FOut[FOutTail], Length(vRaw));
  Inc(FOutTail, Length(vRaw));
end;

function TRALWebSocketChannel.ServerHandshakeStep(const APath: StringRAL): boolean;
var
  vRes: IntegerRAL;
  vText: AnsiString;
begin
  Result := FHandshakeDone;
  if Result then
    Exit;
  repeat
    vRes := Fill;
    if vRes = 0 then
      HandshakeError(emSocketClosed);
  until vRes = RALSocketWouldBlock;
  // the request goes first; refusals need its path and subprotocols
  if (FPath = '') and (FInTail = FInHead) then
    Exit;
  if FPath = '' then
  begin
    if not ServerHandshakeParse then
      Exit;
    if (APath <> '') and (FPath <> APath) then
    begin
      vText := 'HTTP/1.1 404 Not Found'#13#10'Content-Length: 0'#13#10'Connection: close'#13#10#13#10;
      FOutHead := 0;
      FOutTail := 0;
      Inner.Write(Pointer(vText)^, Length(vText));
      HandshakeError(string(FPath));
    end;
    // the client must offer the subprotocol [MQTT-6.0.0-3]
    if (FProtocol <> '') and not ListHas(string(FPeerProtocols), string(FProtocol)) then
    begin
      vText := 'HTTP/1.1 400 Bad Request'#13#10'Content-Length: 0'#13#10'Connection: close'#13#10#13#10;
      FOutHead := 0;
      FOutTail := 0;
      Inner.Write(Pointer(vText)^, Length(vText));
      HandshakeError(emWSNoProtocol);
    end;
  end;
  FlushOut;
  FHandshakeDone := True;
  ParseFrames;
  Result := True;
end;

procedure TRALWebSocketChannel.SendClose(ACode: Word);
var
  vData: array[0..1] of Byte;
begin
  if FCloseSent then
    Exit;
  FCloseSent := True;
  vData[0] := Byte(ACode shr 8);
  vData[1] := Byte(ACode);
  try
    QueueFrame(8, @vData[0], 2);
    FlushOut;
  except
    // the link may be gone already
  end;
end;

procedure TRALWebSocketChannel.Close;
begin
  if FHandshakeDone then
    SendClose(RALWSCloseNormal);
  inherited Close;
end;

function TRALWebSocketChannel.HasBuffered: boolean;
begin
  Result := (FPayloadTail > FPayloadHead) or Inner.HasBuffered;
end;

function TRALWebSocketChannel.Read(var ABuffer; ACount: IntegerRAL): IntegerRAL;
var
  vRes: IntegerRAL;
begin
  if not FHandshakeDone then
  begin
    Result := RALSocketWouldBlock;
    Exit;
  end;
  // a pong or a queued frame goes out whenever the link has room
  if FOutHead < FOutTail then
    FlushOut;
  while (FPayloadTail = FPayloadHead) and (not FClosed) do
  begin
    vRes := Fill;
    if vRes = 0 then
    begin
      FClosed := True;
      Break;
    end;
    if not ParseFrames then
      Break;
    if vRes = RALSocketWouldBlock then
      Break;
  end;
  if FPayloadTail > FPayloadHead then
  begin
    Result := FPayloadTail - FPayloadHead;
    if Result > ACount then
      Result := ACount;
    Move(FPayload[FPayloadHead], ABuffer, Result);
    Inc(FPayloadHead, Result);
    if FPayloadHead = FPayloadTail then
    begin
      FPayloadHead := 0;
      FPayloadTail := 0;
    end;
  end
  else if FClosed then
    Result := 0
  else
    Result := RALSocketWouldBlock;
end;

function TRALWebSocketChannel.WantedEvents: Word;
begin
  Result := Inner.WantedEvents;
  if FOutHead < FOutTail then
    Result := Result or RALPollOut;
end;

function TRALWebSocketChannel.Write(const ABuffer; ACount: IntegerRAL): IntegerRAL;
begin
  // what was queued goes first; new data waits until it is gone
  if FOutHead < FOutTail then
  begin
    FlushOut;
    if FOutHead < FOutTail then
    begin
      Result := RALSocketWouldBlock;
      Exit;
    end;
  end;
  QueueFrame(2, @ABuffer, ACount);
  FlushOut;
  Result := ACount;
end;

end.
