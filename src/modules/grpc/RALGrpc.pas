/// gRPC framing shared by the module and the client: length-prefixed messages, gRPC-Web
/// trailers, status codes, timeouts.
unit RALGrpc;

{$I ..\..\core\PascalRAL.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  RALTypes, RALTools, RALExceptions, RALBase64, RALCompress, RALProtobuf;

const
  /// Status: success.
  RAL_GRPC_OK = 0;
  /// Status: the caller cancelled.
  RAL_GRPC_CANCELLED = 1;
  /// Status: an error with no better code (an exception the handler did not map).
  RAL_GRPC_UNKNOWN = 2;
  /// Status: the request is wrong whatever the state of the system.
  RAL_GRPC_INVALID_ARGUMENT = 3;
  /// Status: the deadline passed before the call ended.
  RAL_GRPC_DEADLINE_EXCEEDED = 4;
  /// Status: something asked for was not found.
  RAL_GRPC_NOT_FOUND = 5;
  /// Status: what the call wanted to create exists.
  RAL_GRPC_ALREADY_EXISTS = 6;
  /// Status: the caller may not do this.
  RAL_GRPC_PERMISSION_DENIED = 7;
  /// Status: a quota or a size ran out.
  RAL_GRPC_RESOURCE_EXHAUSTED = 8;
  /// Status: the system is not in the state the call needs.
  RAL_GRPC_FAILED_PRECONDITION = 9;
  /// Status: aborted, usually by a concurrency conflict.
  RAL_GRPC_ABORTED = 10;
  /// Status: past the valid range.
  RAL_GRPC_OUT_OF_RANGE = 11;
  /// Status: the method does not exist or is not supported.
  RAL_GRPC_UNIMPLEMENTED = 12;
  /// Status: an invariant broke.
  RAL_GRPC_INTERNAL = 13;
  /// Status: try again later.
  RAL_GRPC_UNAVAILABLE = 14;
  /// Status: data lost or corrupted.
  RAL_GRPC_DATA_LOSS = 15;
  /// Status: no valid credentials.
  RAL_GRPC_UNAUTHENTICATED = 16;

  /// The flag of a data frame whose message is compressed.
  cRALGrpcFlagCompressed = $01;
  /// The flag of a gRPC-Web trailer frame.
  cRALGrpcFlagTrailers = $80;
  /// The largest message accepted by default (4 MB, as gRPC's own default).
  cRALGrpcMaxMessage = 4 * 1024 * 1024;

type
  /// The wire form of a call.
  TRALGrpcProtocol = (
    /// gRPC over HTTP/2 with real trailers (application/grpc).
    gpNative,
    /// gRPC-Web, binary (application/grpc-web): trailers in the body, any HTTP version.
    gpWeb,
    /// gRPC-Web as base64 text (application/grpc-web-text), for clients that cannot
    /// read binary.
    gpWebText
  );
  /// A set of wire forms.
  TRALGrpcProtocols = set of TRALGrpcProtocol;

  /// The shape of a method.
  TRALGrpcMethodKind = (gmkUnknown, gmkUnary, gmkServerStream, gmkClientStream, gmkBidiStream);

  /// A call ended with a status other than OK.
  ERALGrpcError = class(ERALException)
  private
    FStatus: IntegerRAL;
    FStatusMessage: StringRAL;
  public
    /// The error of AStatus with AMessage.
    constructor CreateStatus(AStatus: IntegerRAL; const AMessage: StringRAL);

    /// The gRPC status code.
    property Status: IntegerRAL read FStatus;
    /// The status message (grpc-message, decoded).
    property StatusMessage: StringRAL read FStatusMessage;
  end;

  { Reads frames from the bytes of a body as they arrive, in any pieces: the
    5-byte prefix (flags, big-endian length) then the message. For gRPC-Web
    text the bytes are base64 first, read in groups of 4 so that pieces
    padded on their own may follow one another. }
  TRALGrpcFrameReader = class
  private
    /// True for gRPC-Web text (base64).
    FBase64: Boolean;
    /// The bytes written so far.
    FBuffer: TBytes;
    /// Number of bytes held in the buffer.
    FCount: IntegerRAL;
    FMaxMessage: IntegerRAL;
    /// Base64 text not yet decoded.
    FPending: StringRAL;
    /// Offset of the first unread byte.
    FStart: IntegerRAL;

    /// Adds decoded bytes to the buffer.
    procedure Append(const AData; ASize: IntegerRAL);
  public
    /// A reader of binary frames, or of base64 text when ABase64.
    constructor Create(ABase64: Boolean; AMaxMessage: IntegerRAL = cRALGrpcMaxMessage);

    /// Feeds ASize more bytes of the body.
    procedure Feed(const AData; ASize: IntegerRAL);
    { Takes the next whole frame: its flags and its message; False when the
      bytes fed so far end before it. Raises past MaxMessage. }
    function Next(out AFlags: Byte; out AMessage: TBytes): Boolean;
    /// Whether bytes of an unfinished frame remain.
    function Pending: Boolean;

    /// The largest message accepted.
    property MaxMessage: IntegerRAL read FMaxMessage write FMaxMessage;
  end;

/// Base64 of ABytes, for gRPC-Web text.
function RALGrpcBase64(const ABytes: TBytes): StringRAL;
/// The content type of AProtocol (with +proto).
function RALGrpcContentType(AProtocol: TRALGrpcProtocol): StringRAL;
/// Decodes grpc-message (percent-encoded UTF-8).
function RALGrpcDecodeMessage(const AText: StringRAL): StringRAL;
/// Encodes grpc-message: bytes outside printable ASCII, and %, percent-encoded.
function RALGrpcEncodeMessage(const AText: StringRAL): StringRAL;
/// One frame: the prefix and AMessage, compressed with gzip when ACompress.
function RALGrpcFrame(const AMessage: TBytes; ACompress: Boolean = False): TBytes;
/// A message frame of AMessage.
function RALGrpcMessageFrame(AMessage: TRALProtoMessage; ACompress: Boolean = False): TBytes;
/// The protocol of a content type; False when it is not gRPC.
function RALGrpcProtocolOf(const AContentType: StringRAL; out AProtocol: TRALGrpcProtocol): Boolean;
/// The name of a status code (OK, CANCELLED...).
function RALGrpcStatusName(AStatus: IntegerRAL): StringRAL;
/// grpc-timeout as milliseconds; -1 when absent or malformed.
function RALGrpcTimeoutMS(const AValue: StringRAL): Int64;
/// Milliseconds as a grpc-timeout value.
function RALGrpcTimeoutText(AMilliseconds: Int64): StringRAL;
{ The gRPC-Web trailer frame: grpc-status, grpc-message and ATrailers
  ("name: value" lines), each line ended by CR LF. }
function RALGrpcTrailerFrame(AStatus: IntegerRAL; const AMessage: StringRAL;
  ATrailers: TStrings): TBytes;
/// Reads the lines of a trailer frame into AList as name=value, names in lower case.
procedure RALGrpcParseTrailers(const AData: TBytes; AList: TStrings);
/// Undoes gzip on a compressed message (the gzip unit must be linked: RALCompressZLib).
function RALGrpcGunzip(const AData: TBytes): TBytes;

implementation

const
  /// Status names, in code order.
  cStatusNames: array[0..16] of StringRAL = ('OK', 'CANCELLED', 'UNKNOWN',
    'INVALID_ARGUMENT', 'DEADLINE_EXCEEDED', 'NOT_FOUND', 'ALREADY_EXISTS',
    'PERMISSION_DENIED', 'RESOURCE_EXHAUSTED', 'FAILED_PRECONDITION', 'ABORTED',
    'OUT_OF_RANGE', 'UNIMPLEMENTED', 'INTERNAL', 'UNAVAILABLE', 'DATA_LOSS',
    'UNAUTHENTICATED');

/// Compresses or expands ABytes through the registered gzip coder.
function GzipBytes(const ABytes: TBytes; ACompress: Boolean): TBytes;
var
  vClass: TRALCompressClass;
  vCoder: TRALCompress;
  vIn, vOut: TStream;
begin
  vClass := GetCompressClass(ctGZip);
  if vClass = nil then
    raise ERALGrpcError.CreateStatus(RAL_GRPC_UNIMPLEMENTED,
      'gzip is not linked (add RALCompressZLib to the uses)');
  vCoder := vClass.Create;
  vIn := TMemoryStream.Create;
  try
    vCoder.Format := ctGZip;
    if Length(ABytes) > 0 then
      vIn.WriteBuffer(ABytes[0], Length(ABytes));
    vIn.Position := 0;
    if ACompress then
      vOut := vCoder.Compress(vIn)
    else
      vOut := vCoder.Decompress(vIn);
    try
      vOut.Position := 0;
      SetLength(Result, vOut.Size);
      if vOut.Size > 0 then
        vOut.ReadBuffer(Result[0], vOut.Size);
    finally
      vOut.Free;
    end;
  finally
    vIn.Free;
    vCoder.Free;
  end;
end;

{ ERALGrpcError }

constructor ERALGrpcError.CreateStatus(AStatus: IntegerRAL; const AMessage: StringRAL);
begin
  inherited Create(Format('gRPC %s: %s', [string(RALGrpcStatusName(AStatus)), string(AMessage)]));
  FStatus := AStatus;
  FStatusMessage := AMessage;
end;

{ TRALGrpcFrameReader }

constructor TRALGrpcFrameReader.Create(ABase64: Boolean; AMaxMessage: IntegerRAL);
begin
  inherited Create;
  FBase64 := ABase64;
  FMaxMessage := AMaxMessage;
end;

procedure TRALGrpcFrameReader.Append(const AData; ASize: IntegerRAL);
var
  vNew: IntegerRAL;
begin
  if ASize <= 0 then
    Exit;
  // drop what was taken before growing
  if FStart > 0 then
  begin
    if FCount > FStart then
      Move(FBuffer[FStart], FBuffer[0], FCount - FStart);
    Dec(FCount, FStart);
    FStart := 0;
  end;
  if FCount + ASize > Length(FBuffer) then
  begin
    vNew := Length(FBuffer) * 2;
    if vNew < 4096 then
      vNew := 4096;
    while vNew < FCount + ASize do
      vNew := vNew * 2;
    SetLength(FBuffer, vNew);
  end;
  Move(AData, FBuffer[FCount], ASize);
  Inc(FCount, ASize);
end;

procedure TRALGrpcFrameReader.Feed(const AData; ASize: IntegerRAL);
var
  vText: StringRAL;
  vInt, vUsed, vGroup: IntegerRAL;
  vChar: CharRAL;
  vBytes: TBytes;
  vQuantum: StringRAL;
begin
  if not FBase64 then
  begin
    Append(AData, ASize);
    Exit;
  end;
  SetLength(vText, ASize);
  if ASize > 0 then
    Move(AData, vText[1], ASize);
  vText := FPending + vText;
  // whole groups of 4 base64 characters (white space dropped) are decoded now
  vQuantum := '';
  vUsed := 0;
  vGroup := 0;
  for vInt := 1 to Length(vText) do
  begin
    vChar := vText[vInt];
    if vChar in [#9, #10, #13, ' '] then
      Continue;
    RALProtoAppendByte(vQuantum, Ord(vChar));
    Inc(vGroup);
    if vGroup = 4 then
    begin
      vBytes := TRALBase64.DecodeAsBytes(vQuantum);
      if Length(vBytes) > 0 then
        Append(vBytes[0], Length(vBytes));
      vQuantum := '';
      vGroup := 0;
      vUsed := vInt;
    end;
  end;
  FPending := Copy(vText, vUsed + 1, MaxInt);
end;

function TRALGrpcFrameReader.Next(out AFlags: Byte; out AMessage: TBytes): Boolean;
var
  vLength: Cardinal;
begin
  Result := False;
  AFlags := 0;
  AMessage := nil;
  if FCount - FStart < 5 then
    Exit;
  vLength := (Cardinal(FBuffer[FStart + 1]) shl 24) or (Cardinal(FBuffer[FStart + 2]) shl 16) or
    (Cardinal(FBuffer[FStart + 3]) shl 8) or Cardinal(FBuffer[FStart + 4]);
  if (FMaxMessage > 0) and (vLength > Cardinal(FMaxMessage)) then
    raise ERALGrpcError.CreateStatus(RAL_GRPC_RESOURCE_EXHAUSTED,
      StringRAL(Format('message of %d bytes, more than %d', [vLength, FMaxMessage])));
  if Cardinal(FCount - FStart - 5) < vLength then
    Exit;
  AFlags := FBuffer[FStart];
  SetLength(AMessage, vLength);
  if vLength > 0 then
    Move(FBuffer[FStart + 5], AMessage[0], vLength);
  Inc(FStart, 5 + IntegerRAL(vLength));
  Result := True;
end;

function TRALGrpcFrameReader.Pending: Boolean;
begin
  Result := (FCount > FStart) or (RALTrim(FPending) <> '');
end;

{ functions }

function RALGrpcBase64(const ABytes: TBytes): StringRAL;
begin
  Result := TRALBase64.Encode(ABytes);
end;

function RALGrpcContentType(AProtocol: TRALGrpcProtocol): StringRAL;
begin
  case AProtocol of
    gpWeb: Result := 'application/grpc-web+proto';
    gpWebText: Result := 'application/grpc-web-text+proto';
  else
    Result := 'application/grpc+proto';
  end;
end;

function RALGrpcDecodeMessage(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  vInt := 1;
  while vInt <= Length(AText) do
  begin
    if (AText[vInt] = '%') and (vInt + 2 <= Length(AText)) and
       (AText[vInt + 1] in ['0'..'9', 'a'..'f', 'A'..'F']) and
       (AText[vInt + 2] in ['0'..'9', 'a'..'f', 'A'..'F']) then
    begin
      RALProtoAppendByte(Result, StrToInt('$' + string(Copy(AText, vInt + 1, 2))));
      Inc(vInt, 3);
    end
    else
    begin
      RALProtoAppendByte(Result, Ord(AText[vInt]));
      Inc(vInt);
    end;
  end;
end;

function RALGrpcEncodeMessage(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := 1 to Length(AText) do
    if (Ord(AText[vInt]) < $20) or (Ord(AText[vInt]) > $7E) or (AText[vInt] = '%') then
      Result := Result + StringRAL('%' + IntToHex(Ord(AText[vInt]), 2))
    else
      RALProtoAppendByte(Result, Ord(AText[vInt]));
end;

function RALGrpcFrame(const AMessage: TBytes; ACompress: Boolean): TBytes;
var
  vData: TBytes;
  vLength: Cardinal;
begin
  if ACompress then
    vData := GzipBytes(AMessage, True)
  else
    vData := AMessage;
  vLength := Length(vData);
  SetLength(Result, 5 + Length(vData));
  if ACompress then
    Result[0] := cRALGrpcFlagCompressed
  else
    Result[0] := 0;
  Result[1] := Byte(vLength shr 24);
  Result[2] := Byte(vLength shr 16);
  Result[3] := Byte(vLength shr 8);
  Result[4] := Byte(vLength);
  if Length(vData) > 0 then
    Move(vData[0], Result[5], Length(vData));
end;

function RALGrpcGunzip(const AData: TBytes): TBytes;
begin
  Result := GzipBytes(AData, False);
end;

function RALGrpcMessageFrame(AMessage: TRALProtoMessage; ACompress: Boolean): TBytes;
begin
  if AMessage = nil then
    Result := RALGrpcFrame(nil, ACompress)
  else
    Result := RALGrpcFrame(AMessage.Encode, ACompress);
end;

procedure RALGrpcParseTrailers(const AData: TBytes; AList: TStrings);
var
  vText: StringRAL;
  vLines: TStringList;
  vInt, vPos: IntegerRAL;
  vLine: string;
begin
  SetLength(vText, Length(AData));
  if Length(AData) > 0 then
    Move(AData[0], vText[1], Length(AData));
  vLines := TStringList.Create;
  try
    vLines.Text := string(vText);
    for vInt := 0 to vLines.Count - 1 do
    begin
      vLine := vLines[vInt];
      vPos := Pos(':', vLine);
      if vPos > 1 then
        AList.Values[LowerCase(Trim(Copy(vLine, 1, vPos - 1)))] := Trim(Copy(vLine, vPos + 1, MaxInt));
    end;
  finally
    vLines.Free;
  end;
end;

function RALGrpcProtocolOf(const AContentType: StringRAL;
  out AProtocol: TRALGrpcProtocol): Boolean;
var
  vType: StringRAL;
  vPos: IntegerRAL;
begin
  vType := StringRAL(LowerCase(Trim(string(AContentType))));
  vPos := Pos(';', string(vType));
  if vPos > 0 then
    vType := StringRAL(Trim(Copy(string(vType), 1, vPos - 1)));
  Result := True;
  AProtocol := gpNative;
  if (vType = 'application/grpc-web-text') or (vType = 'application/grpc-web-text+proto') then
    AProtocol := gpWebText
  else if (vType = 'application/grpc-web') or (vType = 'application/grpc-web+proto') then
    AProtocol := gpWeb
  else if (vType = 'application/grpc') or (vType = 'application/grpc+proto') then
    AProtocol := gpNative
  else
    Result := False;
end;

function RALGrpcStatusName(AStatus: IntegerRAL): StringRAL;
begin
  if (AStatus >= Low(cStatusNames)) and (AStatus <= High(cStatusNames)) then
    Result := cStatusNames[AStatus]
  else
    Result := StringRAL(IntToStr(AStatus));
end;

function RALGrpcTimeoutMS(const AValue: StringRAL): Int64;
var
  vText: string;
  vNumber: Int64;
begin
  Result := -1;
  vText := Trim(string(AValue));
  if (Length(vText) < 2) or (Length(vText) > 9) then
    Exit;
  if not TryStrToInt64(Copy(vText, 1, Length(vText) - 1), vNumber) or (vNumber < 0) then
    Exit;
  case vText[Length(vText)] of
    'H': Result := vNumber * 3600000;
    'M': Result := vNumber * 60000;
    'S': Result := vNumber * 1000;
    'm': Result := vNumber;
    'u': Result := (vNumber + 999) div 1000;
    'n': Result := (vNumber + 999999) div 1000000;
  end;
end;

function RALGrpcTimeoutText(AMilliseconds: Int64): StringRAL;
begin
  // at most 8 digits
  if AMilliseconds < 100000000 then
    Result := StringRAL(IntToStr(AMilliseconds) + 'm')
  else if AMilliseconds div 1000 < 100000000 then
    Result := StringRAL(IntToStr(AMilliseconds div 1000) + 'S')
  else
    Result := StringRAL(IntToStr(AMilliseconds div 3600000) + 'H');
end;

function RALGrpcTrailerFrame(AStatus: IntegerRAL; const AMessage: StringRAL;
  ATrailers: TStrings): TBytes;
var
  vText: StringRAL;
  vInt: IntegerRAL;
  vLength: Cardinal;
begin
  vText := StringRAL('grpc-status:' + IntToStr(AStatus) + #13#10);
  if AMessage <> '' then
    vText := vText + 'grpc-message:' + RALGrpcEncodeMessage(AMessage) + #13#10;
  if ATrailers <> nil then
    for vInt := 0 to ATrailers.Count - 1 do
      vText := vText + StringRAL(LowerCase(ATrailers.Names[vInt]) + ':' +
        ATrailers.ValueFromIndex[vInt] + #13#10);
  vLength := Length(vText);
  SetLength(Result, 5 + Length(vText));
  Result[0] := cRALGrpcFlagTrailers;
  Result[1] := Byte(vLength shr 24);
  Result[2] := Byte(vLength shr 16);
  Result[3] := Byte(vLength shr 8);
  Result[4] := Byte(vLength);
  if vLength > 0 then
    Move(vText[1], Result[5], vLength);
end;

end.
