{ @abstract Unit for all type definitions used within PascalRAL
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs in this unit
}
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
  IntegerRAL = Integer;
  Int64RAL = Int64;
  DoubleRAL = Double;

  {$IF Defined(FPC) OR Defined(DELPHIXE2UP)}
    UInt64RAL = UInt64;
  {$ELSE}
    UInt64RAL = Int64;
  {$IFEND}

  {$IF Defined(FPC) OR Defined(DELPHI10_1UP)}
    StringRAL = UTF8String;
    CharRAL = UTF8Char;
  {$ELSE}
    StringRAL = UTF8String;
    CharRAL = Char;
  {$IFEND}
  PCharRAL = ^CharRAL;

  {$IF NOT DEFINED(FPC) AND NOT DEFINED(DELPHI2010UP)}
  TBytes = array of byte;
  {$IFEND}

  {$IF DEFINED(DELPHI10_1UP) OR DEFINED(FPC)}
  TRALBufFileStream = TBufferedFileStream;
  {$ELSE}
  TRALBufFileStream = TFileStream;
  {$IFEND}

  TRALCriptoType = (crNone, crAES128, crAES192, crAES256);
  TRALJSONType = (rjtString, rjtNumber, rjtBoolean, rjtObject, rjtArray);
  TRALMethod = (amALL, amGET, amPOST, amPUT, amPATCH, amDELETE, amOPTIONS,
    amHEAD, amTRACE);
  TRALMethods = set of TRALMethod;
  TRALParamKind = (rpkNONE, rpkBODY, rpkFIELD, rpkHEADER, rpkQUERY, rpkCOOKIE);
  TRALParamKinds = set of TRALParamKind;

  { How a param value travels on the wire.

    rptText is what every param has always used: the value becomes UTF-8 text,
    so reading it back is a locale-dependent parse - a client writing 2,5 and a
    server parsing with '.' as the decimal separator silently gets 0, and 03/04
    is March 4th or April 3rd depending on the machine.

    The others carry the raw value instead, little-endian and fixed size, tagged
    with the matching rctRAL* content type: no parse, no locale. The type is
    stated explicitly rather than inferred from the value because Object Pascal
    promotes numeric literals - 2 would fit Integer, Int64 and Double, and
    Currency could never be told apart from Double. }
  TRALParamType = (rptText, rptInteger, rptInt64, rptDouble, rptCurrency,
                   rptBoolean, rptDateTime);

  TRALSecurityOption = (rsoBruteForceProtection, rsoFloodProtection,
    rsoPathTransvBlackList);
  TRALSecurityOptions = set of TRALSecurityOption;
  TRALExecBehavior = (ebSingleThread, ebMultiThread);
  TRALDateTimeFormat = (dtfUnix, dtfISO8601, dtfCustom);

  { How a send attempt ended, from the transport's point of view.

    This is what decides whether resending is safe, so it has to mean the same
    thing on every engine - StatusCode cannot: when no HTTP response happened
    there is no status, and each engine used to leave a different made-up value
    behind (-1 on Indy, 10061 on mORMot2, 0 on fpHTTP).

    The distinction that matters is whether the request reached a server:
    rteConnect means it provably did not, so another BaseURL may be tried with
    any method; rteTimeout means it did and may already have run, so only an
    idempotent method may be sent elsewhere. }
  TRALTransportError = (
    /// an HTTP response was received, even a 4xx/5xx one
    rteNone,
    /// could not connect: refused, DNS, unreachable, connect timeout
    rteConnect,
    /// connected and the request went out; the response did not arrive in time
    rteTimeout,
    /// any other transport failure
    rteOther,
    /// the TLS handshake failed over the server certificate - refused by the
    /// engine's own validation, by SSL.Pin or by OnValidateServerCert
    rteCertificate,
    /// the application refused the attempt from OnBeforeExecute, so nothing
    /// went out and there is nothing to resend
    /// - new values are APPENDED, never inserted: every value above keeps its
    ///   ordinal, and CanSwitchURL's "else" already declines to resend what it
    ///   does not know
    rteCancelled);

  { Which HTTP protocol version to speak.

    RAL does not implement HTTP/2 itself: what frames it is the platform under
    an engine - WinHTTP on Windows, OkHttp under HttpURLConnection on Android -
    and only some engines reach such a platform at all. So this is what the
    client ASKS for; what was actually negotiated comes back in
    TRALResponse.ProtocolVersion, because ALPN may always settle on less.

    rhvDefault leaves every engine with the behaviour it has today, so nothing
    changes for an application that does not ask. Asking rhv2 of an engine
    whose SupportsHTTP2 is False raises on the first request instead of
    quietly falling back - a transport that silently is not what was asked for
    is how one spends an afternoon wondering why nothing got faster.

    In practice HTTP/2 only happens over TLS: both platforms negotiate it by
    ALPN and neither offers the cleartext upgrade (h2c).

    The same type answers the other direction, on TRALRequest/TRALResponse:
    which version the message ACTUALLY travelled on - see ProtocolVersion and
    Protocol on TRALHTTPHeaderInfo, which are two faces of one field and
    therefore cannot disagree. That is why rhv10 exists: nothing can ASK for
    HTTP/1.0, but a server still receives it, and the fpHTTP engine decides
    whether to close the connection by it. }
  TRALHTTPVersion = (
    /// whatever the engine does today - HTTP/1.1 everywhere, at present; also
    /// what a transport that cannot tell the version reports back
    rhvDefault,
    /// HTTP/1.0 - an OBSERVED value only: BeforeSendUrl refuses it as a
    /// request, because no engine can ask a transport for it
    rhv10,
    /// force HTTP/1.1, declining an HTTP/2 the platform might have taken
    rhv11,
    /// ask for HTTP/2, falling back to 1.1 when the server does not offer it
    rhv2);

  {$IF Defined(FPC) or Defined(DELPHIXE3UP)}
  TRALBase64StringHelper = {$IFDEF FPC}type{$ELSE}record{$ENDIF} helper for StringRAL
  public
    function toBase64: StringRAL;
    function fromBase64: StringRAL;
  end;
  {$IFEND}

const
  {$IF Defined(FPC) OR Defined(DELPHIXE3UP)}
  POSINISTR = Low(String);
  {$ELSE}
  POSINISTR = 1;
  {$IFEND}

  // old versions of Delphi that don't have sLineBreak
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHI7UP)}
  sLineBreak = #13#10;
  {$IFEND}
  EmptyStr: StringRAL = StringRAL('');

// Returns the last position of a string
function RALHighStr(const AStr: StringRAL): integer;

function StringToBytesUTF8(const AString: StringRAL): TBytes;
function BytesToStringUTF8(const ABytes: TBytes): StringRAL;

function StringToBytes(const AString: StringRAL): TBytes;
function BytesToString(const ABytes: TBytes): StringRAL;

{$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE6UP)}
function DateToISO8601(const AValue: TDateTime): StringRAL;
function ISO8601ToDate(const AValue: StringRAL): TDateTime;
{$IFEND}

/// The version as it is written on the wire - '1.0', '1.1', '2.0', or '' when
/// the transport could not tell. This is what TRALHTTPHeaderInfo.Protocol
/// hands back, so every engine spells it the same way.
function RALHTTPVersionToStr(AVersion: TRALHTTPVersion): StringRAL;
/// Reads back what RALHTTPVersionToStr writes, and also what the engines find
/// in a request line or a status line: the leading 'HTTP/' is optional, and so
/// is the minor part, so '2', '2.0' and 'HTTP/2' all mean rhv2. Anything else
/// - including the empty string - is rhvDefault, never a guess.
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
  { StringRAL already IS UTF-8: TEncoding.UTF8.GetBytes forced the whole string
    into UTF-16 before the call and encoded it back afterwards - a full round
    trip, two allocations, to hand back exactly the bytes the string already
    held.

    And it was not only slow: the decoder does not refuse an invalid sequence,
    it substitutes. The bytes A3 9A 4F C2 00 7E FF 10 came back as EF BF BD
    EF BF BD 4F EF BF BD 00 7E EF BF BD 10 - half of them destroyed, every
    invalid one collapsing into the same U+FFFD, and the length doubled. A
    derived key is made of bytes like those, which is what RALHashBase works
    around with HMACAsDigest.

    BytesToStringUTF8 already copied straight through (its TEncoding.UTF8
    .GetString is commented out just below); the pair now closes and the round
    trip is byte for byte. }
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
    //Result := TEncoding.UTF8.GetString(ABytes);
//    SetString(Result, PUTF8Char(ABytes), Length(ABytes));
      SetString(Result, PAnsiChar(ABytes), Length(ABytes));
  {$ELSE}
    SetLength(vStr, Length(ABytes));
    Move(ABytes[0], vStr[POSINISTR], Length(ABytes));
    Result := UTF8Decode(vStr);
  {$ENDIF}
end;

{$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE6UP)}
function DateToISO8601(const AValue: TDateTime): StringRAL;
var
  vFmt: TFormatSettings;
begin
  vFmt.DateSeparator := '-';
  vFmt.ShortDateFormat := 'yyyy-mm-dd';
  vFmt.TimeSeparator := ':';
  vFmt.ShortTimeFormat := 'hh:nn:ss';
  vFmt.LongTimeFormat := 'hh:nn:ss.zzz';

  Result := StringReplace(DateToStr(AValue, vFmt), ' ', 'T', []);
end;

function ISO8601ToDate(const AValue: StringRAL): TDateTime;
var
  vFmt: TFormatSettings;
begin
  vFmt.DateSeparator := '-';
  vFmt.ShortDateFormat := 'yyyy-mm-dd';
  vFmt.TimeSeparator := ':';
  vFmt.ShortTimeFormat := 'hh:nn:ss';
  vFmt.LongTimeFormat := 'hh:nn:ss.zzz';

  Result := StrToDate(StringReplace(AValue, 'T', ' ', []), vFmt);
end;
{$IFEND}

function RALHTTPVersionToStr(AVersion: TRALHTTPVersion): StringRAL;
begin
  case AVersion of
    rhv10: Result := '1.0';
    rhv11: Result := '1.1';
    rhv2:  Result := '2.0';
  else
    Result := ''; // rhvDefault: the transport did not say, so neither do we
  end;
end;

function StrToRALHTTPVersion(const AValue: StringRAL): TRALHTTPVersion;
var
  vStr: StringRAL;
  vPos: IntegerRAL;
begin
  vStr := UpperCase(Trim(AValue));

  { 'HTTP/1.1 200 OK' and 'HTTP/1.1' and '1.1' all have to land on rhv11:
    drop the scheme, then whatever follows the version }
  if Pos(StringRAL('HTTP/'), vStr) = 1 then
    Delete(vStr, 1, 5);

  vPos := Pos(StringRAL(' '), vStr);
  if vPos > 0 then
    vStr := Copy(vStr, 1, vPos - 1);

  { 'h2' is how ALPN names it, and what OkHttp reports }
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
