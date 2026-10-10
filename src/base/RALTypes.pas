/// Types that are the same on every compiler, the shared enums, and string conversions.
unit RALTypes;

interface

{$I ..\base\PascalRAL.inc}
{$IFDEF FPC}
{$modeswitch typehelpers}
{$ENDIF}

uses
  {$IFDEF FPC}
  bufstream, LazUTF8,
  {$ENDIF}
  Classes, SysUtils;

type
  /// Integer type used across PascalRAL.
  IntegerRAL = Integer;
  /// 64-bit integer type used across PascalRAL.
  Int64RAL = Int64;
  /// Floating point type used across PascalRAL.
  DoubleRAL = Double;

  {$IF Defined(FPC) OR Defined(DELPHIXE2UP)}
    /// Unsigned 64-bit integer; Int64 on compilers without UInt64.
    UInt64RAL = UInt64;
  {$ELSE}
    /// Unsigned 64-bit integer; Int64 on compilers without UInt64.
    UInt64RAL = Int64;
  {$IFEND}

  {$IF Defined(FPC) OR Defined(DELPHI10_1UP)}
    /// UTF-8 string used for all text in PascalRAL.
    StringRAL = UTF8String;
    /// Character of a StringRAL.
    CharRAL = UTF8Char;
  {$ELSE}
    /// UTF-8 string used for all text in PascalRAL.
    StringRAL = UTF8String;
    /// Character of a StringRAL.
    CharRAL = Char;
  {$IFEND}
  /// Pointer to a CharRAL.
  PCharRAL = ^CharRAL;

  {$IF NOT DEFINED(FPC) AND NOT DEFINED(DELPHI2010UP)}
  /// Dynamic byte array, for compilers that do not declare TBytes.
  TBytes = array of byte;
  {$IFEND}

  {$IF DEFINED(DELPHI10_1UP) OR DEFINED(FPC)}
  /// Buffered file stream; a plain TFileStream where the RTL has no buffered one.
  TRALBufFileStream = TBufferedFileStream;
  {$ELSE}
  /// Buffered file stream; a plain TFileStream where the RTL has no buffered one.
  TRALBufFileStream = TFileStream;
  {$IFEND}

  /// Body cipher: none, or AES with a 128, 192 or 256-bit key.
  TRALCriptoType = (crNone, crAES128, crAES192, crAES256);
  /// Kind of a JSON value.
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);
  /// HTTP method; amALL means every method, amUNKNOWN one the server answers 501.
  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTIONS,
    amHEAD, amTRACE, amUNKNOWN);
  /// Set of HTTP methods.
  TRALMethods = set of TRALMethod;
  /// Where a param travels (body, field, header, query, cookie); rpkNONE is not sent.
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  /// Set of param kinds.
  TRALParamKinds = set of TRALParamKind;

  { How a param value travels: rptText as UTF-8 text, the others as the raw
    little-endian value, tagged with its rctRAL* content type and read back
    without depending on the locale. }
  TRALParamType = (rptText, rptInteger, rptInt64, rptDouble, rptCurrency,
                   rptBoolean, rptDateTime);

  /// Protection against brute force, flood or path traversal.
  TRALSecurityOption = (rsoBruteForceProtection, rsoFloodProtection,
    rsoPathTransvBlackList);
  /// Set of security protections.
  TRALSecurityOptions = set of TRALSecurityOption;
  { Security header added to every answer: X-Content-Type-Options,
    X-Frame-Options, Referrer-Policy, Strict-Transport-Security (under TLS only)
    and Content-Security-Policy. }
  TRALSecurityHeader = (rshContentTypeOptions, rshFrameOptions, rshReferrerPolicy,
    rshStrictTransport, rshContentSecurityPolicy);
  /// Set of security headers.
  TRALSecurityHeaders = set of TRALSecurityHeader;
  /// Thread that runs a client call: the calling one (ebSingleThread) or a new one.
  TRALExecBehavior = (ebSingleThread, ebMultiThread);
  /// Date and time format of a storage: Unix time, ISO 8601 or a custom format.
  TRALDateTimeFormat = (dtfUnix, dtfISO8601, dtfCustom);
  /// IP family of an address.
  TRALIpMode = (rimIPv4, rimIPv6);

  /// How a send attempt ended for the transport; decides whether it may be resent.
  TRALTransportError = (
    /// An HTTP response arrived, even a 4xx or 5xx one.
    rteNone,
    /// No connection: refused, DNS, unreachable or connect timeout.
    rteConnect,
    /// The request went out and no response arrived in time.
    rteTimeout,
    /// Any other transport failure.
    rteOther,
    /// The server certificate was refused (engine, SSL.Pins or OnValidateServerCert).
    rteCertificate,
    /// OnBeforeExecute refused the attempt; nothing was sent.
    rteCancelled);

  { HTTP version a client asks for, or the one a message travelled on
    (TRALHTTPHeaderInfo.ProtocolVersion). }
  TRALHTTPVersion = (
    /// The engine's own choice; also the value when the version is unknown.
    rhvDefault,
    /// HTTP/1.0; only reported, never requested.
    rhv10,
    /// HTTP/1.1, even where HTTP/2 is available.
    rhv11,
    /// HTTP/2, falling back to 1.1 when the server does not offer it.
    rhv2);

  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}
  /// Base64 conversion of a StringRAL.
  TRALBase64StringHelper = {$IFDEF FPC}type{$ELSE}record{$ENDIF} helper for StringRAL
  public
    /// Returns the string encoded in Base64.
    function toBase64: StringRAL;
    /// Returns the string decoded from Base64.
    function fromBase64: StringRAL;
  end;
  {$IFEND}

const
  {$IF Defined(FPC) OR Defined(DELPHIXE3UP)}
  /// Index of the first character of a string.
  POSINISTR = Low(String);
  {$ELSE}
  /// Index of the first character of a string.
  POSINISTR = 1;
  {$IFEND}

  {$IF NOT Defined(FPC) AND NOT Defined(DELPHI7UP)}
  /// Line break, for Delphi versions that do not declare sLineBreak.
  sLineBreak = #13#10;
  {$IFEND}
  /// Empty StringRAL.
  EmptyStr: StringRAL = StringRAL('');

  /// Idempotent methods (RFC 9110 9.2.2): the ones a client may send again.
  RALIdempotentMethods = [amGET, amHEAD, amOPTIONS, amTRACE, amPUT, amDELETE];

/// Returns the index of the last character of AStr.
function RALHighStr(const AStr: StringRAL): integer;

/// Returns the bytes of AString as they are (StringRAL is already UTF-8).
function StringToBytesUTF8(const AString: StringRAL): TBytes;
/// Returns ABytes, UTF-8 text, as a StringRAL.
function BytesToStringUTF8(const ABytes: TBytes): StringRAL;

/// Returns AString converted to the ANSI code page.
function StringToBytes(const AString: StringRAL): TBytes;
/// Returns ABytes, read as ANSI text, as a StringRAL.
function BytesToString(const ABytes: TBytes): StringRAL;

/// Returns the version as written on the wire: '1.0', '1.1', '2.0', or '' if unknown.
function RALHTTPVersionToStr(AVersion: TRALHTTPVersion): StringRAL;
{ Reads a version as RALHTTPVersionToStr writes it, or from a request or status
  line ('HTTP/1.1 200 OK', 'h2'); anything else is rhvDefault. }
function StrToRALHTTPVersion(const AValue: StringRAL): TRALHTTPVersion;

implementation

uses
  RALBase64;

function RALHighStr(const AStr: StringRAL): integer;
begin
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE3UP)}
  Result := Length(AStr);
  {$ELSE}
  Result := High(AStr);
  {$IFEND}
end;

function StringToBytes(const AString: StringRAL): TBytes;
{$IFNDEF HAS_Encoding}
  var
    vStr : ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
    Result := TEncoding.ANSI.GetBytes(AString);
  {$ELSE}
    vStr := AString;
    SetLength(Result, Length(vStr));
    Move(vStr[POSINISTR], Result[0], Length(vStr));
  {$ENDIF}
end;

function StringToBytesUTF8(const AString: StringRAL): TBytes;
begin
  // a plain copy: TEncoding.UTF8 would replace the bytes that are not valid UTF-8
  SetLength(Result, Length(AString));
  if Length(AString) > 0 then
    Move(AString[POSINISTR], Result[0], Length(AString));
end;

function BytesToString(const ABytes: TBytes): StringRAL;
{$IFNDEF HAS_Encoding}
  var
    vStr : ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
    Result := TEncoding.ANSI.GetString(ABytes);
  {$ELSE}
    SetLength(vStr, Length(ABytes));
    Move(ABytes[0], vStr[POSINISTR], Length(ABytes));
    Result := UTF8Decode(vStr);
  {$ENDIF}
end;

function BytesToStringUTF8(const ABytes: TBytes): StringRAL;
{$IFNDEF HAS_Encoding}
  var
    vStr: ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
      SetString(Result, PAnsiChar(ABytes), Length(ABytes));
  {$ELSE}
    SetLength(vStr, Length(ABytes));
    Move(ABytes[0], vStr[POSINISTR], Length(ABytes));
    Result := UTF8Decode(vStr);
  {$ENDIF}
end;

function RALHTTPVersionToStr(AVersion: TRALHTTPVersion): StringRAL;
begin
  case AVersion of
    rhv10: Result := '1.0';
    rhv11: Result := '1.1';
    rhv2:  Result := '2.0';
  else
    Result := ''; // rhvDefault: unknown
  end;
end;

function StrToRALHTTPVersion(const AValue: StringRAL): TRALHTTPVersion;
var
  vStr: StringRAL;
  vPos: IntegerRAL;
begin
  vStr := UpperCase(Trim(AValue));

  // 'HTTP/1.1 200 OK', 'HTTP/1.1' and '1.1' all read as 1.1
  if Pos(StringRAL('HTTP/'), vStr) = 1 then
    Delete(vStr, 1, 5);

  vPos := Pos(StringRAL(' '), vStr);
  if vPos > 0 then
    vStr := Copy(vStr, 1, vPos - 1);

  // 'h2' is the ALPN name, which OkHttp reports
  if (vStr = '2') or (vStr = '2.0') or (vStr = 'H2') or (vStr = 'H2C') then
    Result := rhv2
  else if vStr = '1.1' then
    Result := rhv11
  else if vStr = '1.0' then
    Result := rhv10
  else
    Result := rhvDefault;
end;

{ TRALBase64StringHelper }

{$IF Defined(FPC) or defined(DELPHIXE3UP)}
function TRALBase64StringHelper.fromBase64: StringRAL;
begin
  Result := TRALBase64.Decode(Self);
end;

function TRALBase64StringHelper.toBase64: StringRAL;
begin
  Result := TRALBase64.Encode(Self);
end;
{$IFEND}

end.
