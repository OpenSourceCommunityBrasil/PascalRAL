/// The wire format RAL puts on a QUIC stream, and the only place it is written.
///
/// QUIC carries no HTTP here: a request is a binary frame on a bidirectional
/// stream of its own, and the frame ends where the stream ends - the sender
/// finishes its side (the QUIC FIN) and the reader reads to EOF. There is no
/// length prefix in front of the frame because the transport already delimits
/// it, which is the one thing raw QUIC gives that HTTP/3 does not.
///
/// REQUEST
///   1 byte    method, Ord(TRALMethod)
///   block     route (the path only - the host lives in the connection)
///   headers   count, then that many (name, value) blocks
///   block     body
///
/// RESPONSE
///   2 bytes   status code
///   block     content type
///   headers   count, then that many (name, value) blocks
///   block     body
///
/// A block is a 4 byte little endian length followed by that many bytes.
/// Headers travel as pairs rather than as text: an HTTP shape inside a frame
/// that is not HTTP costs a string built, grown and split again on the other
/// side, and a value carrying a colon or a line break then survives only if
/// the separator sniffer guesses right. As pairs it survives by construction.
///
/// This unit is shared by every engine that speaks it - the MsQuic client on
/// the desktop and the Kwik client on Android - so that the two cannot drift
/// apart. Whoever changes the format changes it here, once, and both ends of
/// every pair follow.
unit RALQuicFrame;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$I ..\base\PascalRAL.inc}

interface

uses
  Classes, SysUtils,
  RALTypes, RALConsts, RALParams, RALRequest, RALResponse, RALCompress,
  RALCripto, RALClient;

const
  /// The largest single block this reader will believe. A size prefix read
  /// from the network is an allocation request from a stranger.
  RALQUIC_MAX_FIELD = 64 * 1024 * 1024;

type
  TRALQuicHeader = record
    Name: StringRAL;
    Value: StringRAL;
  end;

  TRALQuicHeaders = array of TRALQuicHeader;

{ THE PRIMITIVES. Exported because the SERVER side of this format is built from
  the same pieces in the other direction - it decodes a request and encodes a
  response - and a second copy of them is how the two ends stop agreeing. }

/// How many bytes a length-prefixed block of ALength will take on the wire.
function RALQuicBlockSize(ALength: IntegerRAL): IntegerRAL;
/// Writes one length-prefixed block and answers where the next one starts.
function RALQuicPutBlockStr(ADest: PByte; const AText: StringRAL): PByte;
/// Reads one, bounded before it is believed. False when the buffer cannot
/// hold what the prefix announces.
function RALQuicReadBlockStr(ABuf: PByte; ASize: IntegerRAL;
                             var APos: IntegerRAL; out AText: StringRAL): boolean;
/// Reads the header block into AParams as rpkHEADER params.
function RALQuicReadHeaders(ABuf: PByte; ASize: IntegerRAL;
                            var APos: IntegerRAL; AParams: TRALParams): boolean;
/// Every rpkHEADER param of AParams, and how many bytes they will take.
function RALQuicCollectHeaders(AParams: TRALParams; var AHeaders: TRALQuicHeaders;
                               out ACount: IntegerRAL): IntegerRAL;
/// Writes the header block and answers where the next field starts.
function RALQuicPutHeaders(ADest: PByte; const AHeaders: TRALQuicHeaders;
                           ACount: IntegerRAL): PByte;

/// Host and port of a full URL. RALSplitHostPort takes a BARE "host:port" -
/// handing it a whole URL makes the scheme part of the host, and every
/// connection then fails to resolve - so the scheme and the path are cut here
/// first. A URL with no port answers 0; the caller decides what that means.
procedure RALQuicHostPort(const AURL: StringRAL; out AHost: StringRAL;
                          out APort: IntegerRAL);

/// The path of a full URL, which is all the frame carries. Returns '/' when
/// the URL has no path.
function RALQuicRouteFromUrl(const AURL: StringRAL): StringRAL;

/// The headers every QUIC request carries, whatever the engine underneath.
/// The values come from the client rather than from a TRALClient reference so
/// that this unit stays free of the engine that calls it.
procedure RALQuicPrepareRequest(ARequest: TRALRequest;
                                const AHost: StringRAL; APort: IntegerRAL;
                                const AUserAgent: StringRAL;
                                ACompress: TRALCompressType;
                                const AAcceptCompress: StringRAL;
                                const ACriptoKey: StringRAL;
                                ACripto: TRALCriptoType;
                                const ASupportedEncript: StringRAL);

/// The request frame. Call it AFTER the headers that depend on the encoded
/// body are needed - it encodes the body first itself, for the reason in the
/// body of the function.
function RALQuicBuildFrame(ARequest: TRALRequest; AMethod: TRALMethod;
                           const ARoute: StringRAL): TBytes;

/// The response frame into AResponse. False when the frame is malformed, and
/// the caller reports the transport error - only the engine knows how.
function RALQuicParseFrame(AFrame: PByte; ASize: IntegerRAL;
                           AResponse: TRALResponse;
                           const ACriptoKey: StringRAL): boolean;

implementation

function RALQuicBlockSize(ALength: IntegerRAL): IntegerRAL;
begin
  Result := 4 + ALength;
end;

function RALQuicPutBlockStr(ADest: PByte; const AText: StringRAL): PByte;
var
  vLen: Cardinal;
begin
  vLen := Length(AText);
  Move(vLen, ADest^, 4);
  Inc(ADest, 4);
  if vLen > 0 then
  begin
    Move(AText[POSINISTR], ADest^, vLen);
    Inc(ADest, vLen);
  end;
  Result := ADest;
end;

function RALQuicReadBlockStr(ABuf: PByte; ASize: IntegerRAL; var APos: IntegerRAL;
  out AText: StringRAL): boolean;
var
  vLen: Cardinal;
begin
  Result := False;
  AText := '';
  if APos + 4 > ASize then
    Exit;
  Move(PByte(ABuf + APos)^, vLen, 4);
  Inc(APos, 4);
  if (vLen > RALQUIC_MAX_FIELD) or (APos + IntegerRAL(vLen) > ASize) then
    Exit;
  if vLen > 0 then
  begin
    SetLength(AText, vLen);
    Move(PByte(ABuf + APos)^, AText[POSINISTR], vLen);
  end;
  Inc(APos, IntegerRAL(vLen));
  Result := True;
end;

/// AddHeader rather than AddParam: a Set-Cookie also lands as a cookie param,
/// the same rule the Indy and mORMot2 clients follow through AppendParamLine.
function RALQuicReadHeaders(ABuf: PByte; ASize: IntegerRAL; var APos: IntegerRAL;
  AParams: TRALParams): boolean;
var
  vCount, vIndex: Cardinal;
  vName, vValue: StringRAL;
begin
  Result := False;
  if APos + 4 > ASize then
    Exit;
  Move(PByte(ABuf + APos)^, vCount, 4);
  Inc(APos, 4);
  { a count read from the network is a promise, not a fact: the smallest pair
    is eight bytes, so anything above what is left in the buffer is a lie }
  if vCount > Cardinal(ASize - APos) div 8 then
    Exit;
  for vIndex := 1 to vCount do
  begin
    if (not RALQuicReadBlockStr(ABuf, ASize, APos, vName)) or
       (not RALQuicReadBlockStr(ABuf, ASize, APos, vValue)) then
      Exit;
    AParams.AddHeader(vName, vValue);
  end;
  Result := True;
end;

/// Reads every rpkHEADER param once - AsString materialises the value, so
/// asking twice would allocate twice - and answers how many bytes the block
/// will take.
function RALQuicCollectHeaders(AParams: TRALParams; var AHeaders: TRALQuicHeaders;
  out ACount: IntegerRAL): IntegerRAL;
var
  vInt: IntegerRAL;
  vParam: TRALParam;
begin
  ACount := 0;
  Result := 4;
  if AParams = nil then
    Exit;
  if Length(AHeaders) < AParams.Count then
    SetLength(AHeaders, AParams.Count);
  for vInt := 0 to Pred(AParams.Count) do
  begin
    vParam := AParams.Index[vInt];
    if vParam.Kind <> rpkHEADER then
      Continue;
    AHeaders[ACount].Name := vParam.ParamName;
    AHeaders[ACount].Value := vParam.AsString;
    Inc(Result, 8 + Length(AHeaders[ACount].Name) + Length(AHeaders[ACount].Value));
    Inc(ACount);
  end;
end;

function RALQuicPutHeaders(ADest: PByte; const AHeaders: TRALQuicHeaders;
  ACount: IntegerRAL): PByte;
var
  vInt: IntegerRAL;
  vLen: Cardinal;
begin
  vLen := ACount;
  Move(vLen, ADest^, 4);
  Inc(ADest, 4);
  for vInt := 0 to ACount - 1 do
  begin
    ADest := RALQuicPutBlockStr(ADest, AHeaders[vInt].Name);
    ADest := RALQuicPutBlockStr(ADest, AHeaders[vInt].Value);
  end;
  Result := ADest;
end;

procedure RALQuicHostPort(const AURL: StringRAL; out AHost: StringRAL;
  out APort: IntegerRAL);
var
  vValue: StringRAL;
  vPos: IntegerRAL;
begin
  vValue := AURL;
  vPos := Pos(StringRAL('://'), vValue);
  if vPos > 0 then
    vValue := Copy(vValue, vPos + 3, Length(vValue));

  for vPos := POSINISTR to Length(vValue) + POSINISTR - 1 do
    if vValue[vPos] = '/' then
    begin
      vValue := Copy(vValue, POSINISTR, vPos - POSINISTR);
      Break;
    end;

  RALSplitHostPort(vValue, AHost, APort);
end;

function RALQuicRouteFromUrl(const AURL: StringRAL): StringRAL;
var
  vPos, vLen: IntegerRAL;
begin
  Result := '/';
  vPos := Pos(StringRAL('://'), AURL);
  if vPos > 0 then
    vPos := vPos + 3
  else
    vPos := POSINISTR;

  vLen := Length(AURL) + POSINISTR;
  while (vPos < vLen) and (AURL[vPos] <> '/') do
    Inc(vPos);
  if vPos < vLen then
    Result := Copy(AURL, vPos, MaxInt);
end;

procedure RALQuicPrepareRequest(ARequest: TRALRequest;
  const AHost: StringRAL; APort: IntegerRAL; const AUserAgent: StringRAL;
  ACompress: TRALCompressType; const AAcceptCompress: StringRAL;
  const ACriptoKey: StringRAL; ACripto: TRALCriptoType;
  const ASupportedEncript: StringRAL);
var
  vCookies: StringRAL;
begin
  { the frame carries only the path, so the host the request was aimed at goes
    in the header every HTTP client sends - TRALRequest rebuilds its full URL
    from it on the server }
  ARequest.Params.AddParam('Host', AHost + ':' + StringRAL(IntToStr(APort)), rpkHEADER);
  ARequest.Params.AddParam('User-Agent', AUserAgent, rpkHEADER);
  ARequest.ContentCompress := ACompress;
  { Accept-Encoding states what the client can READ, which does not depend on
    whether it compresses what it sends - so it is outside the CompressType
    check, the same rule every other engine follows. }
  ARequest.Params.AddParam('Accept-Encoding', AAcceptCompress, rpkHEADER);
  ARequest.CriptoKey := ACriptoKey;
  ARequest.ContentCripto := ACripto;
  if ACripto <> crNone then
  begin
    ARequest.Params.AddParam('Content-Encription', ARequest.ContentEncription, rpkHEADER);
    ARequest.Params.AddParam('Accept-Encription', ASupportedEncript, rpkHEADER);
  end;
  { the cookies the application set travel as one Cookie header, the way the
    Indy and mORMot2 clients send them; the server reads that header back
    into cookie params }
  vCookies := ARequest.Params.AssignParamsText(rpkCOOKIE, False, '=', '; ');
  if vCookies <> '' then
    ARequest.Params.AddParam('Cookie', vCookies, rpkHEADER);
end;

function RALQuicBuildFrame(ARequest: TRALRequest; AMethod: TRALMethod;
  const ARoute: StringRAL): TBytes;
var
  vHeaders: TRALQuicHeaders;
  vHdrCount, vHdrSize: IntegerRAL;
  vSource: TStream;
  vDest: PByte;
  vBodyLen: IntegerRAL;
begin
  { THE BODY IS ENCODED FIRST, and the headers are built afterwards. Not a
    style choice: RequestStream runs EncodeBody, which decides between a raw
    body and multipart and WRITES BACK ContentType - with the boundary - and
    ContentEncoding, with what it actually compressed. Reading either before
    sends a multipart request with no boundary and a gzipped body with no
    Content-Encoding. }
  vSource := ARequest.RequestStream;
  try
    vBodyLen := 0;
    if vSource <> nil then
      vBodyLen := vSource.Size;

    if ARequest.ContentType <> '' then
      ARequest.Params.AddParam('Content-Type', ARequest.ContentType, rpkHEADER);
    if ARequest.ContentDisposition <> '' then
      ARequest.Params.AddParam('Content-Disposition', ARequest.ContentDisposition, rpkHEADER);
    if ARequest.ContentCompress <> ctNone then
      ARequest.Params.AddParam('Content-Encoding', ARequest.ContentEncoding, rpkHEADER);

    vHdrSize := RALQuicCollectHeaders(ARequest.Params, vHeaders, vHdrCount);

    { one allocation, sized up front, and the body read from the encoded stream
      straight into it }
    SetLength(Result, 1 + RALQuicBlockSize(Length(ARoute)) + vHdrSize + RALQuicBlockSize(vBodyLen));
    vDest := PByte(Result);
    vDest^ := Ord(AMethod);
    Inc(vDest);
    vDest := RALQuicPutBlockStr(vDest, ARoute);
    vDest := RALQuicPutHeaders(vDest, vHeaders, vHdrCount);
    PCardinal(vDest)^ := vBodyLen;
    Inc(vDest, 4);
    if vBodyLen > 0 then
    begin
      vSource.Position := 0;
      vSource.ReadBuffer(vDest^, vBodyLen);
    end;
  finally
    FreeAndNil(vSource);
  end;
end;

function RALQuicParseFrame(AFrame: PByte; ASize: IntegerRAL;
  AResponse: TRALResponse; const ACriptoKey: StringRAL): boolean;
var
  vPos: IntegerRAL;
  vStatus: Word;
  vContentType, vBody: StringRAL;
begin
  Result := False;
  if ASize < 2 then
    Exit;

  Move(AFrame^, vStatus, 2);
  vPos := 2;
  if (not RALQuicReadBlockStr(AFrame, ASize, vPos, vContentType)) or
     (not RALQuicReadHeaders(AFrame, ASize, vPos, AResponse.Params)) or
     (not RALQuicReadBlockStr(AFrame, ASize, vPos, vBody)) then
    Exit;

  AResponse.ContentEncoding := AResponse.ParamByName('Content-Encoding').AsString;
  AResponse.Params.CompressType := AResponse.ContentCompress;

  AResponse.ContentEncription := AResponse.ParamByName('Content-Encription').AsString;
  AResponse.Params.CriptoOptions.CriptType := AResponse.ContentCripto;
  AResponse.Params.CriptoOptions.Key := ACriptoKey;

  AResponse.ContentType := vContentType;
  AResponse.ContentDisposition := AResponse.ParamByName('Content-Disposition').AsString;
  AResponse.StatusCode := vStatus;
  { ProtocolVersion stays rhvDefault: it is a version of HTTP, and QUIC is not
    one - the transport could not tell, which is what rhvDefault means }
  AResponse.ResponseText := vBody;
  Result := True;
end;

end.
