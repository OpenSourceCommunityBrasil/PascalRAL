{ WebSocket (RFC 6455) in two shapes. TRALWebSocketChannel is a channel over
  another one (MQTT over WebSocket): the handshake with an optional subprotocol,
  binary frames as one byte stream, ping/pong and closing with a status code.
  TRALWSCodec is the frames alone, with no I/O, fed the bytes as they arrive
  (WebSocket routes and TRALWebSocketClient): text and binary messages put back
  together from their fragments, UTF-8 checked, control frames between fragments,
  close codes, and permessage-deflate (RFC 7692) without context takeover. }
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
  /// No status code in the close frame (never sent, RFC 6455 7.4.1).
  RALWSCloseNoStatus = 1005;
  /// The connection dropped without a close frame (never sent).
  RALWSCloseAbnormal = 1006;
  /// Data inconsistent with the type of the message (text that is not UTF-8).
  RALWSCloseInvalidData = 1007;
  /// Policy violation (a send queue that stays full).
  RALWSClosePolicy = 1008;
  /// The endpoint met a condition it did not expect.
  RALWSCloseInternalError = 1011;
  /// Opcode of a continuation frame.
  RALWSOpContinuation = 0;
  /// Opcode of a text frame.
  RALWSOpText = 1;
  /// Opcode of a binary frame.
  RALWSOpBinary = 2;
  /// Opcode of a close frame.
  RALWSOpClose = 8;
  /// Opcode of a ping frame.
  RALWSOpPing = 9;
  /// Opcode of a pong frame.
  RALWSOpPong = 10;
  /// Name of the compression extension (RFC 7692).
  RALWSDeflateName = 'permessage-deflate';

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

  /// Kind of a WebSocket message.
  TRALWSMessageKind = (
    /// Text, UTF-8.
    wmkText,
    /// Bytes.
    wmkBinary);

  TRALWSCodec = class;

  /// A whole message arrived (fragments joined, decompressed, UTF-8 checked).
  TRALWSOnMessage = procedure(ASender: TRALWSCodec; AKind: TRALWSMessageKind;
    const AData: TBytes) of object;
  /// A ping or a pong arrived, with its data.
  TRALWSOnControl = procedure(ASender: TRALWSCodec; const AData: TBytes) of object;
  { A close frame arrived: ACode is RALWSCloseNoStatus when it had none. Nothing
    is parsed after it }
  TRALWSOnClose = procedure(ASender: TRALWSCodec; ACode: Word;
    const AReason: StringRAL) of object;

  { The frames of RFC 6455 with no I/O: Feed takes the bytes as they arrive, in
    pieces of any size, and fires an event per message, ping, pong and close;
    the Encode functions give the bytes to send. A breach of the protocol raises
    ERALWebSocketError with the close code it calls for. One thread at a time
    feeds it; the Encode functions keep no state and may run on any thread }
  TRALWSCodec = class
  private
    FCloseReceived: boolean;
    FDeflate: boolean;
    FFragmentSize: IntegerRAL;
    /// Bytes fed and not yet parsed.
    FIn: TBytes;
    /// Start of the unparsed bytes in FIn.
    FInHead: IntegerRAL;
    /// End of the bytes in FIn.
    FInTail: IntegerRAL;
    FMaxMessage: Int64RAL;
    /// Payload of the message being put together.
    FMessage: TBytes;
    /// The message being put together was compressed (RSV1 on its first frame).
    FMessageCompressed: boolean;
    /// Bytes in FMessage.
    FMessageLen: IntegerRAL;
    /// Opcode of the message being put together; 0 when none is.
    FMessageOpcode: Byte;
    FOnClose: TRALWSOnClose;
    FOnMessage: TRALWSOnMessage;
    FOnPing: TRALWSOnControl;
    FOnPong: TRALWSOnControl;
    FRole: TRALWSRole;

    /// Adds ACount bytes of AData to the message being put together.
    procedure AppendMessage(AData: PByte; ACount: IntegerRAL);
    /// The last fragment arrived: decompresses, checks and hands the message out.
    procedure EndMessage;
    /// Decompresses ACount bytes of AData (a message of permessage-deflate).
    function Inflate(AData: PByte; ACount: IntegerRAL): TBytes;
    /// Takes every whole frame out of the bytes fed.
    procedure ParseFrames;
  public
    /// A codec of ARole: a client masks what it sends, a server takes only masked frames.
    constructor Create(ARole: TRALWSRole);

    { The close frame of ACode, with AReason (cut to fit the 125 bytes of a control
      frame); ACode 0 sends a close frame with no status }
    function EncodeClose(ACode: Word; const AReason: StringRAL = ''): TBytes;
    { One frame of AOpcode with ACount bytes of AData; AFinal sets FIN, ACompressed
      RSV1. Masked when the role is client }
    function EncodeFrame(AOpcode: Byte; AData: PByte; ACount: IntegerRAL;
      AFinal: boolean = True; ACompressed: boolean = False): TBytes;
    { The frames of a whole message: compressed when Deflate was agreed, cut into
      fragments of FragmentSize bytes when it is above 0 }
    function EncodeMessage(AKind: TRALWSMessageKind; AData: PByte;
      ACount: IntegerRAL): TBytes;
    { Takes ACount bytes of ABuffer that arrived; fires the events of every frame
      they complete. Raises ERALWebSocketError (CloseCode says why) on a breach }
    procedure Feed(const ABuffer; ACount: IntegerRAL);
    /// Forgets what was fed: a new connection.
    procedure Reset;

    /// A close frame arrived; what follows it is ignored.
    property CloseReceived: boolean read FCloseReceived;
    { permessage-deflate was agreed in the handshake: RSV1 marks compressed
      messages, and EncodeMessage compresses }
    property Deflate: boolean read FDeflate write FDeflate;
    /// Largest fragment EncodeMessage sends; 0 sends each message in one frame.
    property FragmentSize: IntegerRAL read FFragmentSize write FFragmentSize;
    { Largest message accepted, in bytes, compressed or not; past it the
      connection fails with 1009. 0 is no limit }
    property MaxMessage: Int64RAL read FMaxMessage write FMaxMessage;
    /// Fired by a close frame.
    property OnClose: TRALWSOnClose read FOnClose write FOnClose;
    /// Fired by each whole message.
    property OnMessage: TRALWSOnMessage read FOnMessage write FOnMessage;
    /// Fired by a ping; the owner answers it (EncodeFrame RALWSOpPong).
    property OnPing: TRALWSOnControl read FOnPing write FOnPing;
    /// Fired by a pong.
    property OnPong: TRALWSOnControl read FOnPong write FOnPong;
    /// Client or server.
    property Role: TRALWSRole read FRole;
  end;

  /// A message received, on a WebSocket route or by TRALWebSocketClient.
  TRALWebSocketMessage = class
  private
    FData: TBytes;
    FKind: TRALWSMessageKind;
  public
    /// A message of AKind with AData.
    constructor Create(AKind: TRALWSMessageKind; const AData: TBytes);

    /// True for a text message.
    function IsText: boolean;
    /// The message as text (UTF-8).
    function Text: StringRAL;

    /// The bytes of the message.
    property Data: TBytes read FData;
    /// Text or binary.
    property Kind: TRALWSMessageKind read FKind;
  end;

/// Sec-WebSocket-Accept for AKey (RFC 6455 4.2.2).
function RALWSAccept(const AKey: StringRAL): StringRAL;
{ Server side of permessage-deflate: True when one of the offers in AOffers (the
  Sec-WebSocket-Extensions of the request) can be taken, with the answer to send
  in AAnswer. RAL compresses each message alone, so the answer always asks both
  sides for no context takeover }
function RALWSAcceptDeflate(const AOffers: StringRAL; out AAnswer: StringRAL): boolean;
{ Client side of permessage-deflate: True when AAnswer (the
  Sec-WebSocket-Extensions of the 101) agreed to RALWSDeflateOffer, False when it
  is empty; raises ERALWebSocketError when it answered something else }
function RALWSCheckDeflateAnswer(const AAnswer: StringRAL): boolean;
/// The permessage-deflate offer of a client: no context takeover either way.
function RALWSDeflateOffer: StringRAL;
{ True when ACode may travel in a close frame (RFC 6455 7.4: 1000-1003,
  1007-1014 and 3000-4999) }
function RALWSValidCloseCode(ACode: Word): boolean;
/// Text (UTF-8) as the bytes of a message.
function RALWSTextBytes(const AText: StringRAL): TBytes;
/// True when the ACount bytes of AData are well-formed UTF-8 (RFC 3629).
function RALWSValidUTF8(AData: PByte; ACount: IntegerRAL): boolean;

implementation

uses
  RALSHA1, RALBase64, RALTools, RALCompress, RALCompressZLib;

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


type
  { A memory stream that refuses to grow past a size: a compressed message
    cannot be inflated without a limit (a zip bomb) }
  TRALWSCappedStream = class(TMemoryStream)
  private
    /// Largest size; 0 is no limit.
    FLimit: Int64RAL;
  public
    function Write(const ABuffer; ACount: Longint): Longint; override;
  end;

function TRALWSCappedStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  if (FLimit > 0) and (Position + ACount > FLimit) then
    raise ERALWebSocketError.CreateClose(RALWSCloseTooBig,
      Format(emWSBadFrame, ['size']));
  Result := inherited Write(ABuffer, ACount);
end;

/// Raises ERALWebSocketError for a frame that breaks the protocol.
procedure CodecError(ACode: Word; const AReason: string);
begin
  raise ERALWebSocketError.CreateClose(ACode, Format(emWSBadFrame, [AReason]));
end;

function RALWSValidUTF8(AData: PByte; ACount: IntegerRAL): boolean;
var
  vInt, vNeed: IntegerRAL;
  vByte, vLow, vHigh: Byte;
begin
  Result := False;
  vInt := 0;
  while vInt < ACount do
  begin
    vByte := AData[vInt];
    if vByte < $80 then
    begin
      Inc(vInt);
      Continue;
    end;
    // the range of the second byte rules out overlongs, surrogates and > U+10FFFF
    vLow := $80;
    vHigh := $BF;
    if (vByte >= $C2) and (vByte <= $DF) then
      vNeed := 1
    else if (vByte >= $E0) and (vByte <= $EF) then
    begin
      vNeed := 2;
      if vByte = $E0 then
        vLow := $A0
      else if vByte = $ED then
        vHigh := $9F;
    end
    else if (vByte >= $F0) and (vByte <= $F4) then
    begin
      vNeed := 3;
      if vByte = $F0 then
        vLow := $90
      else if vByte = $F4 then
        vHigh := $8F;
    end
    else
      Exit;
    if vInt + vNeed >= ACount then
      Exit;
    if (AData[vInt + 1] < vLow) or (AData[vInt + 1] > vHigh) then
      Exit;
    if (vNeed >= 2) and ((AData[vInt + 2] and $C0) <> $80) then
      Exit;
    if (vNeed = 3) and ((AData[vInt + 3] and $C0) <> $80) then
      Exit;
    Inc(vInt, vNeed + 1);
  end;
  Result := True;
end;

function RALWSValidCloseCode(ACode: Word): boolean;
begin
  Result := ((ACode >= 1000) and (ACode <= 1003)) or
            ((ACode >= 1007) and (ACode <= 1014)) or
            ((ACode >= 3000) and (ACode <= 4999));
end;

/// The parameters of one extension offer ("name; a; b=1"), trimmed, name first.
procedure SplitExtension(const AText: string; AParts: TStrings);
var
  vInt: IntegerRAL;
begin
  AParts.Clear;
  AParts.Delimiter := ';';
  AParts.StrictDelimiter := True;
  AParts.DelimitedText := AText;
  for vInt := 0 to AParts.Count - 1 do
    AParts[vInt] := Trim(AParts[vInt]);
end;

/// The value of the parameter "name=value" or "name" in AParts; '' when absent.
function ExtensionParam(AParts: TStrings; const AName: string; out AFound: boolean): string;
var
  vInt, vPos: IntegerRAL;
  vName: string;
begin
  Result := '';
  AFound := False;
  for vInt := 1 to AParts.Count - 1 do
  begin
    vPos := Pos('=', AParts[vInt]);
    if vPos > 0 then
      vName := Trim(Copy(AParts[vInt], 1, vPos - 1))
    else
      vName := AParts[vInt];
    if SameText(vName, AName) then
    begin
      AFound := True;
      if vPos > 0 then
        Result := StringReplace(Trim(Copy(AParts[vInt], vPos + 1, MaxInt)), '"', '',
          [rfReplaceAll]);
      Exit;
    end;
  end;
end;

function RALWSAcceptDeflate(const AOffers: StringRAL; out AAnswer: StringRAL): boolean;
var
  vOffers, vParts: TStringList;
  vInt, vParam, vBits: IntegerRAL;
  vFound, vGood: boolean;
  vValue, vName: string;
begin
  Result := False;
  AAnswer := '';
  vOffers := TStringList.Create;
  vParts := TStringList.Create;
  try
    vOffers.Delimiter := ',';
    vOffers.StrictDelimiter := True;
    vOffers.DelimitedText := string(AOffers);
    for vInt := 0 to vOffers.Count - 1 do
    begin
      SplitExtension(vOffers[vInt], vParts);
      if (vParts.Count = 0) or not SameText(vParts[0], RALWSDeflateName) then
        Continue;
      vGood := True;
      for vParam := 1 to vParts.Count - 1 do
      begin
        vName := vParts[vParam];
        if Pos('=', vName) > 0 then
          vName := Trim(Copy(vName, 1, Pos('=', vName) - 1));
        if not (SameText(vName, 'server_no_context_takeover') or
                SameText(vName, 'client_no_context_takeover') or
                SameText(vName, 'server_max_window_bits') or
                SameText(vName, 'client_max_window_bits')) then
          vGood := False;
      end;
      // RAL compresses with a window of 4 KB (12 bits): a smaller one cannot be kept
      vValue := ExtensionParam(vParts, 'server_max_window_bits', vFound);
      if vFound then
      begin
        vBits := StrToIntDef(vValue, 0);
        if (vBits < 12) or (vBits > 15) then
          vGood := False;
      end;
      if not vGood then
        Continue;
      AAnswer := RALWSDeflateName + '; server_no_context_takeover; client_no_context_takeover';
      Result := True;
      Exit;
    end;
  finally
    vParts.Free;
    vOffers.Free;
  end;
end;

function RALWSDeflateOffer: StringRAL;
begin
  Result := RALWSDeflateName + '; server_no_context_takeover; client_no_context_takeover';
end;

function RALWSCheckDeflateAnswer(const AAnswer: StringRAL): boolean;
var
  vParts: TStringList;
  vFound: boolean;
begin
  Result := False;
  if Trim(string(AAnswer)) = '' then
    Exit;
  vParts := TStringList.Create;
  try
    SplitExtension(string(AAnswer), vParts);
    if (vParts.Count = 0) or not SameText(vParts[0], RALWSDeflateName) or
       (Pos(',', string(AAnswer)) > 0) then
      HandshakeError('Sec-WebSocket-Extensions');
    // the server must keep no context, or a message alone cannot be inflated
    ExtensionParam(vParts, 'server_no_context_takeover', vFound);
    if not vFound then
      HandshakeError('server_no_context_takeover');
  finally
    vParts.Free;
  end;
  Result := True;
end;

{ TRALWSCodec }

constructor TRALWSCodec.Create(ARole: TRALWSRole);
begin
  inherited Create;
  FRole := ARole;
end;

procedure TRALWSCodec.Reset;
begin
  FIn := nil;
  FInHead := 0;
  FInTail := 0;
  FMessage := nil;
  FMessageLen := 0;
  FMessageOpcode := 0;
  FMessageCompressed := False;
  FCloseReceived := False;
end;

procedure TRALWSCodec.AppendMessage(AData: PByte; ACount: IntegerRAL);
begin
  if ACount <= 0 then
    Exit;
  if (FMaxMessage > 0) and (Int64RAL(FMessageLen) + ACount > FMaxMessage) then
    CodecError(RALWSCloseTooBig, 'size');
  if FMessageLen + ACount > Length(FMessage) then
    SetLength(FMessage, (FMessageLen + ACount) * 2 + 256);
  Move(AData^, FMessage[FMessageLen], ACount);
  Inc(FMessageLen, ACount);
end;

function TRALWSCodec.Inflate(AData: PByte; ACount: IntegerRAL): TBytes;
const
  cTail: array[0..3] of Byte = (0, 0, $FF, $FF);
var
  vCoder: TRALStreamCoder;
  vOut: TRALWSCappedStream;
begin
  vCoder := RALCreateStreamDecoder(ctDeflate);
  if vCoder = nil then
    CodecError(RALWSCloseInternalError, 'deflate');
  vOut := TRALWSCappedStream.Create;
  try
    vOut.FLimit := FMaxMessage;
    try
      // the 4 bytes RFC 7692 7.2.2 has the sender drop, and the receiver put back
      if ACount > 0 then
        vCoder.Update(AData^, ACount, vOut);
      vCoder.Update(cTail, SizeOf(cTail), vOut);
    except
      on e: ERALWebSocketError do
        raise;
      on e: Exception do
        CodecError(RALWSCloseInvalidData, 'deflate');
    end;
    SetLength(Result, vOut.Size);
    if vOut.Size > 0 then
      Move(vOut.Memory^, Result[0], vOut.Size);
  finally
    vOut.Free;
    vCoder.Free;
  end;
end;

procedure TRALWSCodec.EndMessage;
var
  vData: TBytes;
  vKind: TRALWSMessageKind;
begin
  if FMessageCompressed then
    vData := Inflate(PByte(FMessage), FMessageLen)
  else
  begin
    SetLength(vData, FMessageLen);
    if FMessageLen > 0 then
      Move(FMessage[0], vData[0], FMessageLen);
  end;
  if FMessageOpcode = RALWSOpText then
    vKind := wmkText
  else
    vKind := wmkBinary;
  FMessageOpcode := 0;
  FMessageLen := 0;
  FMessageCompressed := False;
  // a large buffer is not kept between messages
  if Length(FMessage) > 65536 then
    FMessage := nil;
  if (vKind = wmkText) and not RALWSValidUTF8(PByte(vData), Length(vData)) then
    CodecError(RALWSCloseInvalidData, 'UTF-8');
  if Assigned(FOnMessage) then
    FOnMessage(Self, vKind, vData);
end;

procedure TRALWSCodec.Feed(const ABuffer; ACount: IntegerRAL);
begin
  if (ACount <= 0) or FCloseReceived then
    Exit;
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
  ParseFrames;
  // what is left is a part of a frame: a large buffer goes once it is used up
  if (FInHead = FInTail) and (Length(FIn) > 65536) then
  begin
    FIn := nil;
    FInHead := 0;
    FInTail := 0;
  end;
end;

procedure TRALWSCodec.ParseFrames;
var
  vAvail, vHeader, vInt, vSize: IntegerRAL;
  vFirst, vSecond, vOpcode: Byte;
  vLen: Int64RAL;
  vMasked, vFinal, vCompressed: boolean;
  vMask: array[0..3] of Byte;
  vData: PByte;
  vBytes: TBytes;
  vCode: Word;
  vReason: StringRAL;
begin
  while not FCloseReceived do
  begin
    vAvail := FInTail - FInHead;
    if vAvail < 2 then
      Exit;
    vFirst := FIn[FInHead];
    vSecond := FIn[FInHead + 1];
    vFinal := (vFirst and $80) <> 0;
    vCompressed := (vFirst and $40) <> 0;
    vOpcode := vFirst and $0F;
    // RSV2 and RSV3: no extension uses them; RSV1 only for permessage-deflate
    if (vFirst and $30) <> 0 then
      CodecError(RALWSCloseProtocolError, 'RSV');
    if vCompressed and ((not FDeflate) or (vOpcode = RALWSOpContinuation) or
       (vOpcode >= RALWSOpClose)) then
      CodecError(RALWSCloseProtocolError, 'RSV1');
    if not (vOpcode in [RALWSOpContinuation, RALWSOpText, RALWSOpBinary,
       RALWSOpClose, RALWSOpPing, RALWSOpPong]) then
      CodecError(RALWSCloseProtocolError, 'opcode');
    vMasked := (vSecond and $80) <> 0;
    // a server takes only masked frames, a client only unmasked ones (5.1)
    if vMasked <> (FRole = mwrServer) then
      CodecError(RALWSCloseProtocolError, 'mask');
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
      // the most significant bit must be 0 (5.2)
      if (FIn[FInHead + 2] and $80) <> 0 then
        CodecError(RALWSCloseProtocolError, 'length');
      vLen := 0;
      for vInt := 2 to 9 do
        vLen := (vLen shl 8) or FIn[FInHead + vInt];
      vHeader := 10;
    end;
    if vOpcode >= RALWSOpClose then
    begin
      // control frames: whole and small (5.5)
      if (not vFinal) or (vLen > 125) then
        CodecError(RALWSCloseProtocolError, 'control');
    end
    else
    begin
      if (vOpcode = RALWSOpContinuation) and (FMessageOpcode = 0) then
        CodecError(RALWSCloseProtocolError, 'continuation');
      if (vOpcode <> RALWSOpContinuation) and (FMessageOpcode <> 0) then
        CodecError(RALWSCloseProtocolError, 'fragment');
      if (FMaxMessage > 0) and (Int64RAL(FMessageLen) + vLen > FMaxMessage) then
        CodecError(RALWSCloseTooBig, 'size');
    end;
    if vLen > MaxInt - 1024 then
      CodecError(RALWSCloseTooBig, 'size');
    if vMasked then
    begin
      if vAvail < vHeader + 4 then
        Exit;
      Move(FIn[FInHead + vHeader], vMask[0], 4);
      Inc(vHeader, 4);
    end;
    if vAvail < vHeader + vLen then
      Exit;

    vSize := IntegerRAL(vLen);
    vData := @FIn[FInHead + vHeader];
    if vMasked then
      for vInt := 0 to vSize - 1 do
        vData[vInt] := vData[vInt] xor vMask[vInt and 3];
    Inc(FInHead, vHeader + vSize);

    case vOpcode of
      RALWSOpText, RALWSOpBinary, RALWSOpContinuation:
      begin
        if vOpcode <> RALWSOpContinuation then
        begin
          FMessageOpcode := vOpcode;
          FMessageCompressed := vCompressed;
        end;
        AppendMessage(vData, vSize);
        if vFinal then
          EndMessage;
      end;
      RALWSOpClose:
      begin
        if vSize = 1 then
          CodecError(RALWSCloseProtocolError, 'close');
        vCode := RALWSCloseNoStatus;
        vReason := '';
        if vSize >= 2 then
        begin
          vCode := (Word(vData[0]) shl 8) or vData[1];
          if not RALWSValidCloseCode(vCode) then
            CodecError(RALWSCloseProtocolError, 'close code');
          if not RALWSValidUTF8(@vData[2], vSize - 2) then
            CodecError(RALWSCloseInvalidData, 'UTF-8');
          SetString(vReason, PAnsiChar(@vData[2]), vSize - 2);
        end;
        FCloseReceived := True;
        if Assigned(FOnClose) then
          FOnClose(Self, vCode, vReason);
        Exit;
      end;
      RALWSOpPing, RALWSOpPong:
      begin
        SetLength(vBytes, vSize);
        if vSize > 0 then
          Move(vData^, vBytes[0], vSize);
        if (vOpcode = RALWSOpPing) and Assigned(FOnPing) then
          FOnPing(Self, vBytes)
        else if (vOpcode = RALWSOpPong) and Assigned(FOnPong) then
          FOnPong(Self, vBytes);
      end;
    end;
  end;
end;

function TRALWSCodec.EncodeFrame(AOpcode: Byte; AData: PByte; ACount: IntegerRAL;
  AFinal, ACompressed: boolean): TBytes;
var
  vLen, vInt: IntegerRAL;
  vMask: TBytes;
begin
  if ACount < 0 then
    ACount := 0;
  vLen := 2;
  if ACount >= 65536 then
    vLen := 10
  else if ACount >= 126 then
    vLen := 4;
  if FRole = mwrClient then
    Inc(vLen, 4);
  SetLength(Result, vLen + ACount);
  Result[0] := AOpcode and $0F;
  if AFinal then
    Result[0] := Result[0] or $80;
  if ACompressed then
    Result[0] := Result[0] or $40;
  if ACount >= 65536 then
  begin
    Result[1] := 127;
    for vInt := 0 to 3 do
      Result[2 + vInt] := 0;
    Result[6] := Byte(ACount shr 24);
    Result[7] := Byte(ACount shr 16);
    Result[8] := Byte(ACount shr 8);
    Result[9] := Byte(ACount);
  end
  else if ACount >= 126 then
  begin
    Result[1] := 126;
    Result[2] := Byte(ACount shr 8);
    Result[3] := Byte(ACount);
  end
  else
    Result[1] := ACount;
  if ACount > 0 then
    Move(AData^, Result[vLen], ACount);
  if FRole = mwrClient then
  begin
    Result[1] := Result[1] or $80;
    vMask := RandomBytes(4);
    Move(vMask[0], Result[vLen - 4], 4);
    for vInt := 0 to ACount - 1 do
      Result[vLen + vInt] := Result[vLen + vInt] xor vMask[vInt and 3];
  end;
end;

function TRALWSCodec.EncodeClose(ACode: Word; const AReason: StringRAL): TBytes;
var
  vData: TBytes;
  vLen: IntegerRAL;
begin
  if ACode = 0 then
  begin
    Result := EncodeFrame(RALWSOpClose, nil, 0);
    Exit;
  end;
  vLen := Length(AReason);
  if vLen > 123 then
    vLen := 123;
  SetLength(vData, 2 + vLen);
  vData[0] := Byte(ACode shr 8);
  vData[1] := Byte(ACode);
  if vLen > 0 then
    Move(Pointer(AReason)^, vData[2], vLen);
  Result := EncodeFrame(RALWSOpClose, PByte(vData), Length(vData));
end;

function TRALWSCodec.EncodeMessage(AKind: TRALWSMessageKind; AData: PByte;
  ACount: IntegerRAL): TBytes;
var
  vCoder: TRALStreamCoder;
  vOut: TMemoryStream;
  vPayload: TBytes;
  vData: PByte;
  vSize, vPos, vPiece, vTotal: IntegerRAL;
  vOpcode: Byte;
  vFrame: TBytes;
  vCompressed: boolean;
begin
  vData := AData;
  vSize := ACount;
  vCompressed := False;
  // an empty message goes as it is: RSV1 is the sender's choice per message
  if FDeflate and (ACount > 0) then
  begin
    vCoder := RALCreateStreamEncoder(ctDeflate);
    if vCoder <> nil then
    begin
      vOut := TMemoryStream.Create;
      try
        vCoder.Update(AData^, ACount, vOut);
        SetLength(vPayload, vOut.Size);
        if vOut.Size > 0 then
          Move(vOut.Memory^, vPayload[0], vOut.Size);
      finally
        vOut.Free;
        vCoder.Free;
      end;
      // the sync flush ends with 00 00 FF FF, which is not sent (7.2.1)
      vSize := Length(vPayload);
      if (vSize >= 4) and (vPayload[vSize - 4] = 0) and (vPayload[vSize - 3] = 0) and
         (vPayload[vSize - 2] = $FF) and (vPayload[vSize - 1] = $FF) then
        Dec(vSize, 4);
      vData := PByte(vPayload);
      vCompressed := True;
    end;
  end;
  if AKind = wmkText then
    vOpcode := RALWSOpText
  else
    vOpcode := RALWSOpBinary;
  if (FFragmentSize <= 0) or (vSize <= FFragmentSize) then
  begin
    Result := EncodeFrame(vOpcode, vData, vSize, True, vCompressed);
    Exit;
  end;
  Result := nil;
  vTotal := 0;
  vPos := 0;
  while vPos < vSize do
  begin
    vPiece := vSize - vPos;
    if vPiece > FFragmentSize then
      vPiece := FFragmentSize;
    vFrame := EncodeFrame(vOpcode, @vData[vPos], vPiece, vPos + vPiece >= vSize,
      vCompressed and (vPos = 0));
    SetLength(Result, vTotal + Length(vFrame));
    Move(vFrame[0], Result[vTotal], Length(vFrame));
    Inc(vTotal, Length(vFrame));
    Inc(vPos, vPiece);
    vOpcode := RALWSOpContinuation;
  end;
end;

function RALWSTextBytes(const AText: StringRAL): TBytes;
begin
  SetLength(Result, Length(AText));
  if AText <> '' then
    Move(Pointer(AText)^, Result[0], Length(AText));
end;

{ TRALWebSocketMessage }

constructor TRALWebSocketMessage.Create(AKind: TRALWSMessageKind; const AData: TBytes);
begin
  inherited Create;
  FKind := AKind;
  FData := AData;
end;

function TRALWebSocketMessage.IsText: boolean;
begin
  Result := FKind = wmkText;
end;

function TRALWebSocketMessage.Text: StringRAL;
begin
  SetLength(Result, Length(FData));
  if Length(FData) > 0 then
    Move(FData[0], Pointer(Result)^, Length(FData));
end;

end.
