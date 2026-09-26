/// Unit for General public functions
unit RALTools;

{$I ..\base\PascalRAL.inc}

interface

uses
  {$IFDEF RALWindows}
    Windows,
  {$ENDIF}
  {$IFDEF FPC}
    UTF8Process,
  {$ENDIF}
  Classes, SysUtils, Variants, StrUtils, TypInfo, DateUtils,
  RALTypes, RALConsts, RALCompress;

function CriptoToStrCripto(ACripto: TRALCriptoType): StringRAL;
function FixRoute(ARoute: StringRAL): StringRAL;
function HTTPMethodToRALMethod(AMethod: StringRAL): TRALMethod;
function OnlyNumbers(const AValue: StringRAL): StringRAL;
function RALMethodToHTTPMethod(AMethod: TRALMethod): StringRAL;
function RALStringToDateTime(const AValue: StringRAL;
                             const AFormat: StringRAL = 'yyyyMMddhhnnsszzz'): TDateTime;
function RandomBytes(numOfBytes: IntegerRAL): TBytes;
function StrCriptoToCripto(const AStr: StringRAL): TRALCriptoType;

function RALDateTimeToGMT(ADateTime: TDateTime): TDateTime;
/// The inverse of RALDateTimeToGMT: a UTC value back to the local zone
function RALGMTToDateTime(ADateTime: TDateTime): TDateTime;
/// ISO 8601 with milliseconds, the same on every compiler. AInputIsUTC stamps
/// 'Z' on the value as it is (what DateToISO8601 does by default); False says
/// the value is local time and writes the real offset ('-03:00')
function RALDateTimeToISO8601(const AValue: TDateTime; AInputIsUTC: Boolean): StringRAL;
/// Reads what RALDateTimeToISO8601 writes, and ISO 8601 from elsewhere, in the
/// extended (2026-09-22T06:51:24) or the basic format (20260922T065124): 'Z' is
/// kept as written (RAL stamped it on local time for years), an explicit offset
/// is brought to local time, no zone is taken as it is. Raises when the text is
/// not a date; see RALTryISO8601ToDateTime. The ISO 8601 functions of RAL live
/// here only - use these, not the RTL's DateToISO8601/ISO8601ToDate, which
/// XE2..XE5 lack and which differ between Delphi and FPC
function RALISO8601ToDateTime(const AValue: StringRAL): TDateTime;
function RALTryISO8601ToDateTime(const AValue: StringRAL; out ADate: TDateTime): Boolean;
function Contains(const AStr: StringRAL; const AArray: array of StringRAL): boolean;
function RALCPUCount: integer;
function HTTPDateTimeToDateTime(const Astr: StringRAL): TDateTime;
/// Equality in constant time, for MACs and signatures: it does not stop at
/// the first differing byte, so the time taken says nothing about the data
function RALSameBytes(const A, B: TBytes): Boolean;
/// Same thing for secrets kept as strings (passwords, signatures)
function RALSameSecret(const A, B: StringRAL): Boolean;
/// Case-insensitive name comparison without leaving StringRAL. Use it for param
/// and header names; SameText is the general-purpose one and stays for text.
function RALSameName(const A, B: StringRAL): Boolean;
/// A number that came as text over HTTP, whatever the locale of this machine:
/// '2.5' and '2,5' are both 2.5, and with both separators present the last one
/// is the decimal ('1.234,5' and '1,234.5'). False when it is not a number
function RALTryStrToFloat(const AValue: StringRAL; out AResult: Double): Boolean;
/// Same rule as RALTryStrToFloat, for Currency
function RALTryStrToCurr(const AValue: StringRAL; out AResult: Currency): Boolean;
/// Drops trailing blanks without leaving StringRAL. Same rule as the RTL's
/// TrimRight - everything up to and including a space goes - and the same
/// result: a byte above 127 is always part of a multibyte character and never
/// compares below 33, so scanning bytes can never cut one in half.
function RALTrimRight(const A: StringRAL): StringRAL;
/// The same, both ends.
function RALTrim(const A: StringRAL): StringRAL;

/// Atomic counters, spelled the same way on both compilers: Delphi has
/// AtomicIncrement/AtomicDecrement in the RTL, FPC calls them InterLocked* and
/// only declares the 64-bit pair on 64-bit CPUs. All three return the NEW value
/// - InterLockedExchangeAdd, which FPC does have, returns the old one.
function RALAtomicInc(var ATarget: IntegerRAL): IntegerRAL; overload;
function RALAtomicDec(var ATarget: IntegerRAL): IntegerRAL; overload;
/// Adds to a 64-bit counter. Only the addition is atomic: a reader still sees
/// the value move under it, which is what the statistics counters expect.
function RALAtomicInc(var ATarget: Int64RAL; AValue: Int64RAL): Int64RAL; overload;

implementation

{ AtomicIncrement/AtomicDecrement are intrinsics from Delphi XE3 on; before that
  TInterlocked does the same, also returning the new value }
{$IF NOT DEFINED(FPC) AND NOT DEFINED(DELPHIXE3UP)}
uses
  SyncObjs;
{$IFEND}

{$IF DEFINED(FPC) AND NOT DEFINED(CPU64)}
var
  gAtomic64: System.TRTLCriticalSection;
{$IFEND}

const
  { method names without the 'am' prefix, in the order of the enum }
  RALMethodNames: array [TRALMethod] of StringRAL = (
    'ALL', 'GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD', 'TRACE');

function RALTrimRight(const A: StringRAL): StringRAL;
var
  vLast: IntegerRAL;
begin
  { on Delphi TrimRight has no AnsiString overload, so a StringRAL goes UTF-8 ->
    UTF-16 -> UTF-8 around it: two conversions and two heap allocations to drop
    blanks that are ASCII by definition. This one only ever copies when there is
    something to drop. }
  vLast := RALHighStr(A);
  while (vLast >= POSINISTR) and (Ord(A[vLast]) <= 32) do
    Dec(vLast);
  if vLast = RALHighStr(A) then
    Result := A
  else
    Result := Copy(A, POSINISTR, vLast - POSINISTR + 1);
end;

function RALTrim(const A: StringRAL): StringRAL;
var
  vFirst, vLast: IntegerRAL;
begin
  vFirst := POSINISTR;
  vLast := RALHighStr(A);
  while (vFirst <= vLast) and (Ord(A[vFirst]) <= 32) do
    Inc(vFirst);
  while (vLast >= vFirst) and (Ord(A[vLast]) <= 32) do
    Dec(vLast);
  if (vFirst = POSINISTR) and (vLast = RALHighStr(A)) then
    Result := A
  else
    Result := Copy(A, vFirst, vLast - vFirst + 1);
end;

function RALDateTimeToISO8601(const AValue: TDateTime; AInputIsUTC: Boolean): StringRAL;
var
  vOffset: Integer;
  vSign: Char;
begin
  Result := StringRAL(FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz', AValue));
  if AInputIsUTC then
  begin
    Result := Result + 'Z';
    Exit;
  end;
  // minutes east of UTC at that date - summer time included
  vOffset := Round((AValue - RALDateTimeToGMT(AValue)) * MinsPerDay);
  if vOffset < 0 then
    vSign := '-'
  else
    vSign := '+';
  vOffset := Abs(vOffset);
  Result := Result + StringRAL(Format('%s%.2d:%.2d', [vSign, vOffset div 60,
    vOffset mod 60]));
end;

function RALTryISO8601ToDateTime(const AValue: StringRAL; out ADate: TDateTime): Boolean;
var
  vText: string;
  vPos, vLen: Integer;
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec, vZoneH, vZoneM, vDigits: Integer;
  vTime: TDateTime;
  vSign: Char;
  vExtended: Boolean;

  function Digits(ACount: Integer; out AResult: Integer): Boolean;
  var
    vInt: Integer;
  begin
    Result := vPos + ACount - 1 <= vLen;
    AResult := 0;
    if not Result then
      Exit;
    for vInt := vPos to vPos + ACount - 1 do
    begin
      if (vText[POSINISTR - 1 + vInt] < '0') or (vText[POSINISTR - 1 + vInt] > '9') then
        Exit(False);
      AResult := AResult * 10 + Ord(vText[POSINISTR - 1 + vInt]) - Ord('0');
    end;
    Inc(vPos, ACount);
  end;

  function Skip(AChar: Char): Boolean;
  begin
    Result := (vPos <= vLen) and (vText[POSINISTR - 1 + vPos] = AChar);
    if Result then
      Inc(vPos);
  end;

begin
  Result := False;
  ADate := 0;
  vText := Trim(string(AValue));
  vLen := Length(vText);
  vPos := 1;
  vHour := 0;
  vMin := 0;
  vSec := 0;
  vMSec := 0;

  { the extended format (2026-09-22T06:51:24) and the basic one (20260922T065124),
    which the RTL's ISO8601ToDate also reads; the date decides which, and the
    time follows it }
  if not Digits(4, vYear) then
    Exit;
  vExtended := Skip('-');
  if not (Digits(2, vMonth) and ((not vExtended) or Skip('-')) and Digits(2, vDay)) then
    Exit;
  if not TryEncodeDate(vYear, vMonth, vDay, ADate) then
    Exit;

  if Skip('T') or Skip(' ') then
  begin
    if not (Digits(2, vHour) and ((not vExtended) or Skip(':')) and Digits(2, vMin)) then
      Exit;
    if vExtended then
    begin
      if Skip(':') and not Digits(2, vSec) then
        Exit;
    end
    else if (vPos <= vLen) and (vText[POSINISTR - 1 + vPos] >= '0') and
            (vText[POSINISTR - 1 + vPos] <= '9') and not Digits(2, vSec) then
      Exit;
    // a fraction of any length; milliseconds is what TDateTime keeps
    if Skip('.') or Skip(',') then
    begin
      vDigits := 0;
      while (vPos <= vLen) and (vText[POSINISTR - 1 + vPos] >= '0') and (vText[POSINISTR - 1 + vPos] <= '9') do
      begin
        if vDigits < 3 then
          vMSec := vMSec * 10 + Ord(vText[POSINISTR - 1 + vPos]) - Ord('0');
        Inc(vDigits);
        Inc(vPos);
      end;
      while vDigits < 3 do
      begin
        vMSec := vMSec * 10;
        Inc(vDigits);
      end;
    end;
    if not TryEncodeTime(vHour, vMin, vSec, vMSec, vTime) then
      Exit;
    ADate := ADate + vTime;
  end;

  if vPos > vLen then
    Exit(True);                          // no zone: as it is

  if Skip('Z') or Skip('z') then
    Exit(vPos > vLen);                   // 'Z': as written

  vSign := vText[POSINISTR - 1 + vPos];
  if (vSign <> '+') and (vSign <> '-') then
    Exit;
  Inc(vPos);
  if not Digits(2, vZoneH) then
    Exit;
  Skip(':');
  vZoneM := 0;
  if (vPos <= vLen) and not Digits(2, vZoneM) then
    Exit;
  if vPos <= vLen then
    Exit;

  // the offset says where the clock was: back to UTC, then to local time here
  if vSign = '+' then
    ADate := ADate - (vZoneH * 60 + vZoneM) / MinsPerDay
  else
    ADate := ADate + (vZoneH * 60 + vZoneM) / MinsPerDay;
  ADate := RALGMTToDateTime(ADate);
  Result := True;
end;

function RALISO8601ToDateTime(const AValue: StringRAL): TDateTime;
begin
  if not RALTryISO8601ToDateTime(AValue, Result) then
    raise EConvertError.CreateFmt(emISO8601Invalid, [string(AValue)]);
end;

{ HTTP carries numbers as text with no locale attached. Parsing them with the
  settings of the process made the same binary read "2.5" as 0 on a pt-BR server
  (decimal comma) and as 2.5 on an en-US one - and a browser's number input
  always sends the dot. Both separators are accepted; the thousands one, when
  present, is whichever is not the last. }
function RALNormalizeNumber(const AValue: StringRAL): string;
var
  vInt, vLastDot, vLastComma: IntegerRAL;
  vDec: Char;
  vChar: Char;
begin
  Result := Trim(string(AValue));
  vLastDot := 0;
  vLastComma := 0;
  for vInt := 1 to Length(Result) do
    if Result[POSINISTR - 1 + vInt] = '.' then
      vLastDot := vInt
    else if Result[POSINISTR - 1 + vInt] = ',' then
      vLastComma := vInt;
  if (vLastDot = 0) and (vLastComma = 0) then
    Exit;
  if vLastComma > vLastDot then
    vDec := ','
  else
    vDec := '.';
  vInt := 1;
  while vInt <= Length(Result) do
  begin
    vChar := Result[POSINISTR - 1 + vInt];
    if (vChar = '.') or (vChar = ',') then
    begin
      if vChar <> vDec then
      begin
        Delete(Result, vInt, 1);
        Continue;
      end;
      Result[POSINISTR - 1 + vInt] := '.';
    end;
    Inc(vInt);
  end;
end;

function RALInvariantFormat: TFormatSettings;
begin
  Result := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator := ',';
end;

function RALTryStrToFloat(const AValue: StringRAL; out AResult: Double): Boolean;
begin
  Result := TryStrToFloat(RALNormalizeNumber(AValue), AResult, RALInvariantFormat);
  if not Result then
    AResult := 0;
end;

function RALTryStrToCurr(const AValue: StringRAL; out AResult: Currency): Boolean;
begin
  Result := TryStrToCurr(RALNormalizeNumber(AValue), AResult, RALInvariantFormat);
  if not Result then
    AResult := 0;
end;
function RALSameName(const A, B: StringRAL): Boolean;
var
  vInt, vHighA, vHighB: IntegerRAL;
  vA, vB: Byte;
begin
  { byte by byte, without leaving StringRAL: on Delphi SameText has no
    AnsiString overload and converts BOTH sides from UTF-8 to UTF-16 on every
    call - two heap allocations per comparison, in a lookup that runs once per
    param inserted. FPC does not convert, but the ASCII path is shorter there
    too. }
  vHighA := RALHighStr(A);
  vHighB := RALHighStr(B);

  vInt := POSINISTR;
  while (vInt <= vHighA) and (vInt <= vHighB) do
  begin
    vA := Ord(A[vInt]);
    vB := Ord(B[vInt]);

    { outside ASCII, case equivalence belongs to the RTL and not to us: a
      dotless 'i' and 'I' have different UTF-8 lengths and may still match.
      Hand the decision back instead of risking a different answer }
    if (vA > 127) or (vB > 127) then
    begin
      Result := SameText(A, B);
      Exit;
    end;

    if vA <> vB then
    begin
      if (vA >= Ord('a')) and (vA <= Ord('z')) then
        Dec(vA, 32);
      if (vB >= Ord('a')) and (vB <= Ord('z')) then
        Dec(vB, 32);
      if vA <> vB then
      begin
        Result := False;
        Exit;
      end;
    end;
    Inc(vInt);
  end;

  { only reached when everything compared was ASCII and equal - then the
    length decides, and in ASCII a byte and a character are the same thing }
  Result := Length(A) = Length(B);
end;

function FixRoute(ARoute: StringRAL): StringRAL;
var
  vInt, vOut, vHigh: IntegerRAL;
  vPrevSlash: boolean;
begin
  Result := '/' + ARoute;

  { path transversal fix - same semantics as StringReplace(...,'../','',
    rfReplaceAll): it scans left to right and, on a match, carries on AFTER the
    removed run without re-examining what is left. Written by hand because
    StringReplace on Delphi converts the whole string to UTF-16 and back, and
    FixRoute runs once per route evaluated on every request. The Pos in front
    lets the common case - a route with no dot at all - leave without rebuilding
    anything. }
  if Pos(StringRAL('..'), Result) > 0 then
  begin
    vHigh := RALHighStr(Result);
    vOut := POSINISTR - 1;
    vInt := POSINISTR;
    while vInt <= vHigh do
    begin
      if (vInt + 2 <= vHigh) and (Result[vInt] = '.') and
         (Result[vInt + 1] = '.') and (Result[vInt + 2] = '/') then
      begin
        vInt := vInt + 3;
      end
      else
      begin
        Inc(vOut);
        if vOut <> vInt then
          Result[vOut] := Result[vInt];
        Inc(vInt);
      end;
    end;
    SetLength(Result, vOut - POSINISTR + 1);
  end;

  { the "while Pos('//') > 0 do StringReplace" rebuilt the whole string on
    every turn: quadratic in the number of slashes, and a URI carrying
    thousands of them was a denial of service on its own }
  vHigh := RALHighStr(Result);
  vOut := POSINISTR - 1;
  vPrevSlash := False;
  for vInt := POSINISTR to vHigh do
  begin
    if Result[vInt] = '/' then
    begin
      if vPrevSlash then
        Continue;
      vPrevSlash := True;
    end
    else
    begin
      vPrevSlash := False;
    end;
    Inc(vOut);
    if vOut <> vInt then
      Result[vOut] := Result[vInt];
  end;
  SetLength(Result, vOut - POSINISTR + 1);

  if (Result <> '') and (Result <> '/') and (Result[RALHighStr(Result)] = '/') then
    SetLength(Result, Length(Result) - 1);
end;

function RALSameBytes(const A, B: TBytes): Boolean;
var
  vInt, vDiff: IntegerRAL;
begin
  vDiff := Length(A) xor Length(B);
  for vInt := 0 to High(A) do
    if vInt <= High(B) then
      vDiff := vDiff or (A[vInt] xor B[vInt])
    else
      vDiff := vDiff or A[vInt];
  Result := vDiff = 0;
end;

function RALSameSecret(const A, B: StringRAL): Boolean;
var
  vInt, vDiff: IntegerRAL;
begin
  { "=" on strings stops at the first differing character, so a wrong
    password that shares a longer prefix with the real one took longer to be
    refused - enough, over many tries, to guess it character by character }
  vDiff := Length(A) xor Length(B);
  for vInt := POSINISTR to RALHighStr(A) do
    if vInt <= RALHighStr(B) then
      vDiff := vDiff or (Ord(A[vInt]) xor Ord(B[vInt]))
    else
      vDiff := vDiff or Ord(A[vInt]);
  Result := vDiff = 0;
end;

{$IFDEF RALWindows}
{ RtlGenRandom: the system's cryptographic generator, without pulling CryptoAPI }
function SystemFunction036(ABuffer: Pointer; ALength: LongWord): Boolean; stdcall;
  external 'advapi32.dll' name 'SystemFunction036';
{$ENDIF}

function RandomBytes(numOfBytes: IntegerRAL): TBytes;
{$IFNDEF RALWindows}
var
  vFile: TFileStream;
{$ENDIF}
begin
  SetLength(Result, numOfBytes);
  if numOfBytes <= 0 then
    Exit;

  { Randomize + Random reseeded from the clock on every call: two calls in the
    same millisecond gave the same bytes, and the nonce, the token id and now
    the AES IV came out guessable. These are the platform's cryptographic
    sources instead. }
  {$IFDEF RALWindows}
  if not SystemFunction036(@Result[0], numOfBytes) then
    raise Exception.Create(emRandomBytesFailed);
  {$ELSE}
  vFile := TFileStream.Create('/dev/urandom', fmOpenRead or fmShareDenyNone);
  try
    vFile.ReadBuffer(Result[0], numOfBytes);
  finally
    vFile.Free;
  end;
  {$ENDIF}
end;

function HTTPMethodToRALMethod(AMethod: StringRAL): TRALMethod;
var
  vMethod: TRALMethod;
begin
  { a table instead of GetEnumValue: the RTTI version built 'am' + UpperCase -
    two UTF-8/UTF-16 conversions on Delphi, plus a concatenation - and only then
    walked the enum names comparing strings, once per request. The accepted set
    is the same, and an unknown method still becomes amGET }
  Result := amGET;
  for vMethod := Low(TRALMethod) to High(TRALMethod) do
  begin
    if RALSameName(AMethod, RALMethodNames[vMethod]) then
    begin
      Result := vMethod;
      Break;
    end;
  end;
end;

function RALMethodToHTTPMethod(AMethod: TRALMethod): StringRAL;
begin
  { GetEnumName hands back a 'string' (UTF-16 on Delphi) and still needed a
    Delete to drop the 'am' prefix. GetAllowMethods calls it nine times in a
    row }
  Result := RALMethodNames[AMethod];
end;

function StrCriptoToCripto(const AStr: StringRAL): TRALCriptoType;
begin
  if SameText(AStr, 'aes128cbc_pkcs7') then
    Result := crAES128
  else if SameText(AStr, 'aes192cbc_pkcs7') then
    Result := crAES192
  else if SameText(AStr, 'aes256cbc_pkcs7') then
    Result := crAES256
  else
    Result := crNone;
end;

function CriptoToStrCripto(ACripto: TRALCriptoType): StringRAL;
begin
  case ACripto of
    crNone: Result := '';
    crAES128: Result := 'aes128cbc_pkcs7';
    crAES192: Result := 'aes192cbc_pkcs7';
    crAES256: Result := 'aes256cbc_pkcs7';
  end;
end;

function OnlyNumbers(const AValue: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := '';
  for vInt := POSINISTR to RALHighStr(AValue) do
  begin
    {$IF (DEFINED(FPC) OR DEFINED(DELPHI2010UP))}
    if CharInSet(AValue[vInt], ['0'..'9']) then
      Result := Result + AValue[vInt];
    {$ELSE}
    if AValue[vInt] in ['0'..'9'] then
      Result := Result + AValue[vInt];
    {$IFEND}
  end;
end;

function RALStringToDateTime(const AValue: StringRAL; const AFormat: StringRAL): TDateTime;
var
  vInt1, vInt2: integer;
  sAno, sMes, sDia, sHor, sMin, sSeg, sMil: StringRAL;
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  sAno := '0';
  sMes := '0';
  sDia := '0';
  sHor := '0';
  sMin := '0';
  sSeg := '0';
  sMil := '0';

  vInt2 := POSINISTR;
  for vInt1 := POSINISTR to RALHighStr(AFormat) do
  begin
    if vInt2 <= RALHighStr(AValue) then
    begin
      case UpCase(AFormat[vInt1]) of
        'D': sDia := sDia + AValue[vInt2];
        'M': sMes := sMes + AValue[vInt2];
        'A': sAno := sAno + AValue[vInt2];
        'Y': sAno := sAno + AValue[vInt2];
        'H': sHor := sHor + AValue[vInt2];
        'N': sMin := sMin + AValue[vInt2];
        'I': sMin := sMin + AValue[vInt2]; // php
        'S': sSeg := sSeg + AValue[vInt2];
        'Z': sMil := sMil + AValue[vInt2];
      end;
      vInt2 := vInt2 + 1;
    end
    else
    begin
      Break;
    end;
  end;

  wAno := StrToInt(sAno);
  wMes := StrToInt(sMes);
  wDia := StrToInt(sDia);
  wHor := StrToInt(sHor);
  wMin := StrToInt(sMin);
  wSeg := StrToInt(sSeg);
  wMil := StrToInt(sMil);

  if (wAno = 0) or (wMes = 0) or (wDia = 0) then
  begin
    if not TryEncodeTime(wHor, wMin, wSeg, wMil, Result) then
      Result := 0;
  end
  else
  begin
    if not TryEncodeDateTime(wAno, wMes, wDia, wHor, wMin, wSeg, wMil, Result) then
      Result := 0;
  end;
end;

function RALDateTimeToGMT(ADateTime: TDateTime): TDateTime;
  {$IF (NOT DEFINED(FPC)) AND (NOT DEFINED(DELPHIXE2UP))}
var
  vTimeZone: TTimeZoneInformation;
  vBias: cardinal;
  {$IFEND}
begin
  {$IFDEF FPC}
    Result := LocalTimeToUniversal(ADateTime);
  {$ELSE}
    {$IFDEF DELPHIXE2UP}
        Result := TTimeZone.Local.ToUniversalTime(ADateTime);
    {$ELSE}
    case GetTimeZoneInformation(vTimeZone) of
      TIME_ZONE_ID_UNKNOWN:
        vBias := vTimeZone.Bias;
      TIME_ZONE_ID_STANDARD:
        vBias := vTimeZone.Bias + vTimeZone.StandardBias;
      TIME_ZONE_ID_DAYLIGHT:
        vBias := vTimeZone.Bias + vTimeZone.DaylightBias;
      else
        vBias := 0;
    end;
    Result := IncMinute(ADateTime, -vBias);
    {$ENDIF}
  {$ENDIF}
end;

function RALGMTToDateTime(ADateTime: TDateTime): TDateTime;
  {$IF (NOT DEFINED(FPC)) AND (NOT DEFINED(DELPHIXE2UP))}
var
  vTimeZone: TTimeZoneInformation;
  vBias: cardinal;
  {$IFEND}
begin
  {$IFDEF FPC}
    Result := UniversalTimeToLocal(ADateTime);
  {$ELSE}
    {$IFDEF DELPHIXE2UP}
        Result := TTimeZone.Local.ToLocalTime(ADateTime);
    {$ELSE}
    case GetTimeZoneInformation(vTimeZone) of
      TIME_ZONE_ID_UNKNOWN:
        vBias := vTimeZone.Bias;
      TIME_ZONE_ID_STANDARD:
        vBias := vTimeZone.Bias + vTimeZone.StandardBias;
      TIME_ZONE_ID_DAYLIGHT:
        vBias := vTimeZone.Bias + vTimeZone.DaylightBias;
      else
        vBias := 0;
    end;
    Result := IncMinute(ADateTime, vBias);
    {$ENDIF}
  {$ENDIF}
end;

function Contains(const AStr: StringRAL; const AArray: array of StringRAL): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to Pred(Length(AArray)) do
    if RALSameName(AStr, AArray[I]) then
    begin
      Result := True;
      Break;
    end;
end;

function HTTPDateTimeToDateTime(const AStr: StringRAL): TDateTime;
const
  Months: array[1..12] of string = (
    'Jan','Feb','Mar','Apr','May','Jun',
    'Jul','Aug','Sep','Oct','Nov','Dec'
  );
var
  Day, Month, Year, Hour, Min, Sec, i: Integer;
  MonthStr: string;
begin
  // Mon, 27 Jul 2026 20:22:11 GMT
  // M o n ,   2 7   J  u  l     2  0  2  6     2  0  :  2  2  :  1  1     G  M  T
  // 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
  Day      := StrToInt(Copy(AStr, 6, 2));
  MonthStr := Copy(AStr, 9, 3);
  Year     := StrToInt(Copy(AStr, 13, 4));
  Hour     := StrToInt(Copy(AStr, 18, 2));
  Min      := StrToInt(Copy(AStr, 21, 2));
  Sec      := StrToInt(Copy(AStr, 24, 2));

  Month := 0;
  for i := 1 to 12 do
    if SameText(MonthStr, Months[i]) then
    begin
      Month := i;
      Break;
    end;

  if Month = 0 then
    raise EConvertError.CreateFmt(emHTTPDateInvalidMonth, [string(AStr)]);

  Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
end;

function RALCPUCount: integer;
{$IFNDEF FPC}
  {$IFDEF DELPHIXE2UP}
  begin
    Result := CPUCount;
  {$ELSE}
  var
    info: TSystemInfo;
  begin
    FillChar(info, SizeOf(info), 0);
    GetSystemInfo(info);
    Result := info.dwNumberOfProcessors;
  {$ENDIF}
{$ELSE}
begin
  Result := GetSystemThreadCount;
{$ENDIF}
end;

function RALAtomicInc(var ATarget: IntegerRAL): IntegerRAL;
begin
  {$IFDEF FPC}
  Result := InterLockedIncrement(ATarget);
  {$ELSE}
  {$IFDEF DELPHIXE3UP}
  Result := AtomicIncrement(ATarget);
  {$ELSE}
  Result := TInterlocked.Increment(ATarget);
  {$ENDIF}
  {$ENDIF}
end;

function RALAtomicDec(var ATarget: IntegerRAL): IntegerRAL;
begin
  {$IFDEF FPC}
  Result := InterLockedDecrement(ATarget);
  {$ELSE}
  {$IFDEF DELPHIXE3UP}
  Result := AtomicDecrement(ATarget);
  {$ELSE}
  Result := TInterlocked.Decrement(ATarget);
  {$ENDIF}
  {$ENDIF}
end;

function RALAtomicInc(var ATarget: Int64RAL; AValue: Int64RAL): Int64RAL;
begin
  {$IFDEF FPC}
    {$IFDEF CPU64}
    Result := InterLockedExchangeAdd64(ATarget, AValue) + AValue;
    {$ELSE}
    { 32-bit FPC declares no 64-bit interlocked primitive at all (rtl/i386/i386.inc
      has only the longint ones), and a 64-bit write can tear there, so this one
      serialises instead of pretending to be lock free. }
    System.EnterCriticalSection(gAtomic64);
    try
      ATarget := ATarget + AValue;
      Result := ATarget;
    finally
      System.LeaveCriticalSection(gAtomic64);
    end;
    {$ENDIF}
  {$ELSE}
  {$IFDEF DELPHIXE3UP}
  Result := AtomicIncrement(ATarget, AValue);
  {$ELSE}
  Result := TInterlocked.Add(ATarget, AValue);
  {$ENDIF}
  {$ENDIF}
end;

{$IF DEFINED(FPC) AND NOT DEFINED(CPU64)}
initialization
  System.InitCriticalSection(gAtomic64);

finalization
  System.DoneCriticalSection(gAtomic64);
{$IFEND}

end.
