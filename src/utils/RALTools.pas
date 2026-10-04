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
    {$IFDEF UNIX}
    BaseUnix, // RALFileInfo
    {$ENDIF}
  {$ENDIF}
  {$IF Defined(POSIX) and not Defined(FPC)}
    Posix.SysStat, // RALFileInfo
    Posix.Unistd,  // FileClose is inline over close()
  {$IFEND}
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
/// The moment an HTTP date stands for, in UTC; raises EConvertError when the
/// text is not one - see RALTryHTTPDate
function HTTPDateTimeToDateTime(const Astr: StringRAL): TDateTime;
/// An HTTP date the way RFC 9110 5.6.7 asks a recipient to read one - the IMF
/// date ('Sun, 06 Nov 1994 08:49:37 GMT'), RFC 850's ('Sunday, 06-Nov-94
/// 08:49:37 GMT') and asctime's ('Sun Nov  6 08:49:37 1994') - plus what
/// cookies carry besides ('Sun, 06-Nov-1994 08:49:37 GMT'), by the algorithm
/// of RFC 6265 5.1.1. AUtc is the moment in UTC; False when the text holds no
/// date, which makes a header carrying it count as not sent
function RALTryHTTPDate(const AText: StringRAL; out AUtc: TDateTime): Boolean;
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
/// A header or cookie as RFC 9110 5.5 wants it on the wire: CR, LF and NUL
/// become a space. A value an application takes from a request - a file name
/// in Content-Disposition, a cookie, an echoed header - would otherwise end the
/// line where the client chose and write headers, or a body, of its own
/// (response splitting). Every engine sends its headers and cookies through
/// this. The same string comes back when there is nothing to replace.
function RALSafeHeaderText(const AText: StringRAL): StringRAL;
/// An HTTP date (RFC 9110 5.6.7) of a moment already in UTC:
/// 'Sun, 06 Nov 1994 08:49:37 GMT'. Digits and English names only - the RTL's
/// FormatDateTime puts the locale's time separator where ':' is written
function RALHTTPDate(AUtc: TDateTime): StringRAL;
/// Whether AFileName is a regular file - not a folder - with its size and the
/// moment it was last written, in Unix seconds (UTC). A link is followed, as
/// FileExists does. One call to the system, where FileExists, a size and a
/// date take three
function RALFileInfo(const AFileName: string; out ASize: Int64;
  out AModified: Int64): boolean;
/// Copies the published properties ASource and ADest have in common - what a
/// form would store - so that an AssignTo is one line and a property added
/// later is copied without anyone having to remember it. A sub-object
/// (TStrings, a collection, options) is copied INTO the one ADest has; a
/// component is a reference, and the reference is what goes over
procedure RALAssignProperties(ASource, ADest: TPersistent);
/// The setter of a property whose object the class created and frees: copies
/// AValue into AOwned and never takes the pointer, which leaked the owned one
/// and left the class holding an object its caller may free. nil and AOwned
/// itself are ignored
procedure RALAssignOwned(AOwned, AValue: TPersistent);

/// Atomic counters, spelled the same way on both compilers: Delphi has
/// AtomicIncrement/AtomicDecrement in the RTL, FPC calls them InterLocked* and
/// only declares the 64-bit pair on 64-bit CPUs. All three return the NEW value
/// - InterLockedExchangeAdd, which FPC does have, returns the old one.
function RALAtomicInc(var ATarget: IntegerRAL): IntegerRAL; overload;
function RALAtomicDec(var ATarget: IntegerRAL): IntegerRAL; overload;
/// Adds to a 64-bit counter. Only the addition is atomic: a reader still sees
/// the value move under it, which is what the statistics counters expect.
function RALAtomicInc(var ATarget: Int64RAL; AValue: Int64RAL): Int64RAL; overload;

var
  /// How a number goes on the wire as text: '.' for decimals, ',' for
  /// thousands, the rest as the RTL starts with. A local TFormatSettings with
  /// only the separators assigned held whatever the stack had in every other
  /// field, which FloatToStr happened not to read. Filled when the program
  /// starts and only read after that; passed as a const parameter it is not
  /// copied
  RALInvariantFormat: TFormatSettings;

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
    'ALL', 'GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD', 'TRACE',
    '');

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

function RALSafeHeaderText(const AText: StringRAL): StringRAL;
var
  vInt: IntegerRAL;
begin
  Result := AText;
  for vInt := POSINISTR to RALHighStr(Result) do
    if Result[vInt] in [#0, #10, #13] then
      Result[vInt] := ' ';
end;

function RALHTTPDate(AUtc: TDateTime): StringRAL;
const
  cDays: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  cMonths: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  vYear, vMonth, vDay, vHour, vMin, vSec, vMSec: Word;
begin
  DecodeDateTime(AUtc, vYear, vMonth, vDay, vHour, vMin, vSec, vMSec);
  Result := StringRAL(Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
    [cDays[DayOfWeek(AUtc)], vDay, cMonths[vMonth], vYear, vHour, vMin, vSec]));
end;

{$IFDEF RALWindows}
type
  { WIN32_FILE_ATTRIBUTE_DATA, declared here because the RTLs of the two
    compilers do not agree on its name }
  TRALFileAttributeData = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
  end;

{ the wide one on both compilers: FPC's string is UTF-8, which the ANSI entry
  point would read in the system code page }
function RALGetFileAttributesExW(lpFileName: PWideChar; fInfoLevelId: Integer;
  lpFileInformation: Pointer): BOOL; stdcall;
  external 'kernel32.dll' name 'GetFileAttributesExW';

{ a FILETIME counts 100 ns from 1601-01-01, Unix time seconds from 1970 }
function FileTimeToUnixSecs(const ATime: TFileTime): Int64;
begin
  Result := ((Int64(ATime.dwHighDateTime) shl 32) or ATime.dwLowDateTime);
  Result := (Result - 116444736000000000) div 10000000;
end;
{$ENDIF}

function RALFileInfo(const AFileName: string; out ASize: Int64;
  out AModified: Int64): boolean;
{$IFDEF RALWindows}
var
  vData: TRALFileAttributeData;
  vName: UnicodeString;
  vFile: TFileStream;
  vTime: TFileTime;
begin
  ASize := 0;
  AModified := 0;
  vName := UnicodeString(AFileName);
  Result := RALGetFileAttributesExW(PWideChar(vName), 0 {GetFileExInfoStandard},
              @vData) and ((vData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0);
  if not Result then
    Exit;

  if (vData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) = 0 then
  begin
    ASize := (Int64(vData.nFileSizeHigh) shl 32) or vData.nFileSizeLow;
    AModified := FileTimeToUnixSecs(vData.ftLastWriteTime);
    Exit;
  end;

  { a link describes itself - its own size and date - while opening it
    follows it to the file, which is what FileExists answers for }
  try
    vFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      ASize := vFile.Size;
      if GetFileTime(vFile.Handle, nil, nil, @vTime) then
        AModified := FileTimeToUnixSecs(vTime);
    finally
      vFile.Free;
    end;
  except
    on EFOpenError do
      Result := False; // a link to nothing
  end;
end;
{$ELSE}
{$IFDEF FPC}
var
  vStat: TStat;
begin
  ASize := 0;
  AModified := 0;
  Result := (FpStat(PChar(AFileName), vStat) = 0) and (not fpS_ISDIR(vStat.st_mode));
  if Result then
  begin
    ASize := vStat.st_size;
    AModified := vStat.st_mtime;
  end;
end;
{$ELSE}
var
  vStat: _stat;
  vName: UTF8String;
begin
  ASize := 0;
  AModified := 0;
  vName := UTF8String(AFileName);
  Result := (stat(MarshaledAString(vName), vStat) = 0) and (not S_ISDIR(vStat.st_mode));
  if Result then
  begin
    ASize := vStat.st_size;
    AModified := vStat.st_mtime;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure RALAssignProperties(ASource, ADest: TPersistent);
var
  vClass: TClass;
  vList: PPropList;
  vCount, vInt: Integer;
  vProp: PPropInfo;
  vType: PTypeInfo;
  vObj, vOwn: TObject;
begin
  if (ASource = nil) or (ADest = nil) or (ASource = ADest) then
    Exit;
  { the properties of the closest class both are: a PPropInfo only means
    something to the class that declares it and to its descendants }
  vClass := ASource.ClassType;
  while not ADest.InheritsFrom(vClass) do
    vClass := vClass.ClassParent;
  vCount := GetPropList(vClass.ClassInfo, vList);
  try
    for vInt := 0 to vCount - 1 do
    begin
      vProp := vList^[vInt];
      vType := vProp^.PropType{$IFNDEF FPC}^{$ENDIF};
      if vProp^.GetProc = nil then
        Continue;
      if vType^.Kind = tkClass then
      begin
        vObj := GetObjectProp(ASource, vProp);
        if GetTypeData(vType)^.ClassType.InheritsFrom(TComponent) then
        begin
          if vProp^.SetProc <> nil then
            SetObjectProp(ADest, vProp, vObj);
        end
        else
        begin
          vOwn := GetObjectProp(ADest, vProp);
          if (vObj is TPersistent) and (vOwn is TPersistent) and (vOwn <> vObj) then
            TPersistent(vOwn).Assign(TPersistent(vObj));
        end;
        Continue;
      end;
      if vProp^.SetProc = nil then
        Continue;
      case vType^.Kind of
        tkInteger, tkChar, tkWChar, tkEnumeration, tkSet{$IFDEF FPC}, tkBool, tkUChar{$ENDIF}:
          SetOrdProp(ADest, vProp, GetOrdProp(ASource, vProp));
        tkInt64{$IFDEF FPC}, tkQWord{$ENDIF}:
          SetInt64Prop(ADest, vProp, GetInt64Prop(ASource, vProp));
        tkFloat:
          SetFloatProp(ADest, vProp, GetFloatProp(ASource, vProp));
        tkString, tkLString, tkWString, tkUString{$IFDEF FPC}, tkAString{$ENDIF}:
          SetStrProp(ADest, vProp, GetStrProp(ASource, vProp));
        tkVariant:
          SetVariantProp(ADest, vProp, GetVariantProp(ASource, vProp));
        tkMethod:
          SetMethodProp(ADest, vProp, GetMethodProp(ASource, vProp));
      end;
    end;
  finally
    FreeMem(vList);
  end;
end;

procedure RALAssignOwned(AOwned, AValue: TPersistent);
begin
  if (AValue <> nil) and (AValue <> AOwned) then
    AOwned.Assign(AValue);
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
  vInt, vOut, vLastDot, vLastComma: IntegerRAL;
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
  { one pass, compacting in place: the write index never passes the read one.
    A Delete per thousands separator shifted the rest of the string every
    time, so a value of a million separators - one form field, no size limit
    by default - cost about 10^12 character moves, minutes of a core per
    request }
  vOut := 0;
  for vInt := 1 to Length(Result) do
  begin
    vChar := Result[POSINISTR - 1 + vInt];
    if (vChar = '.') or (vChar = ',') then
    begin
      if vChar <> vDec then
        Continue;
      vChar := '.';
    end;
    Inc(vOut);
    Result[POSINISTR - 1 + vOut] := vChar;
  end;
  SetLength(Result, vOut);
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

    { above 127 the bytes are compared as they are, and that is SameText's own
      answer: CompareText folds case for 'a'..'z' only, on both compilers, so
      two valid UTF-8 names it calls equal are equal byte for byte. Handing
      those to SameText cost two UTF-8/UTF-16 conversions to learn nothing }
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

  { only reached when every byte compared was equal, ASCII letters folded -
    then the length decides }
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

{$IFNDEF RALWindows}
var
  { /dev/urandom, opened by the first RandomBytes and kept: opening and closing
    it on every call cost three system calls per AES IV, multipart boundary and
    token nonce, several per request. -1 until it is opened }
  gURandom: IntegerRAL = -1;

function CompareExchangeInt(var ATarget: IntegerRAL; AValue,
  AComparand: IntegerRAL): IntegerRAL;
begin
  {$IFDEF FPC}
  Result := InterLockedCompareExchange(ATarget, AValue, AComparand);
  {$ELSE}
  {$IFDEF DELPHIXE3UP}
  Result := AtomicCmpExchange(ATarget, AValue, AComparand);
  {$ELSE}
  Result := TInterlocked.CompareExchange(ATarget, AValue, AComparand);
  {$ENDIF}
  {$ENDIF}
end;

{ the kept handle, opened by whichever thread gets here first - the others
  close theirs }
function URandomHandle: IntegerRAL;
var
  vHandle: THandle;
begin
  Result := gURandom;
  if Result >= 0 then
    Exit;
  vHandle := FileOpen('/dev/urandom', fmOpenRead or fmShareDenyNone);
  if vHandle = THandle(-1) then
    Exit;
  Result := IntegerRAL(vHandle);
  if CompareExchangeInt(gURandom, Result, -1) <> -1 then
  begin
    FileClose(vHandle);
    Result := gURandom;
  end;
end;

{ reads until ACount bytes arrived: a read from the device may return fewer }
function URandomRead(AHandle: IntegerRAL; var ABytes: TBytes;
  ACount: IntegerRAL): boolean;
var
  vDone, vRead: IntegerRAL;
begin
  vDone := 0;
  while vDone < ACount do
  begin
    vRead := FileRead(THandle(AHandle), ABytes[vDone], ACount - vDone);
    if vRead <= 0 then
      Break;
    Inc(vDone, vRead);
  end;
  Result := vDone = ACount;
end;
{$ENDIF}

function RandomBytes(numOfBytes: IntegerRAL): TBytes;
{$IFNDEF RALWindows}
var
  vHandle: IntegerRAL;
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
  vHandle := URandomHandle;
  if (vHandle >= 0) and URandomRead(vHandle, Result, numOfBytes) then
    Exit;

  { the kept handle could not be had, or failed: the way it always was, which
    raises as it always did }
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
    walked the enum names comparing strings, once per request.
    A method that is not one of these is amUNKNOWN, which the server answers
    with 501. It used to become amGET - and run the route's GET handler, with
    AllowedMethods and SkipAuthMethods judging a GET nobody sent - while 'ALL',
    which is no HTTP method at all, became amALL }
  Result := amUNKNOWN;
  for vMethod := Succ(amALL) to Pred(amUNKNOWN) do
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
  vInt, vOut: IntegerRAL;
begin
  { written in place, one allocation - concatenating a digit at a time
    reallocated the whole result for each one }
  SetLength(Result, Length(AValue));
  vOut := POSINISTR;
  for vInt := POSINISTR to RALHighStr(AValue) do
    if AValue[vInt] in ['0'..'9'] then
    begin
      Result[vOut] := AValue[vInt];
      Inc(vOut);
    end;
  SetLength(Result, vOut - POSINISTR);
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

{ The offset of the DATE being converted, not today's: across a daylight-saving
  change the two differ by an hour. Delphi XE2 on has it in TTimeZone, with
  each year's own rules; FPC and Delphi XE took the offset in force when the
  call ran, so a date on the other side of a change came out an hour off - a
  JWT exp, a JSON date. On Windows they now ask the rules of that year. FPC
  off Windows still has nothing date-aware in its RTL and keeps today's
  offset. }
{$IF (DEFINED(FPC) OR NOT DEFINED(DELPHIXE2UP)) AND DEFINED(RALWindows)}
type
  { SYSTEMTIME as Windows lays it out. Not TSystemTime: FPC's SysUtils
    declares one of its own with DayOfWeek in another place, and which of the
    two that name means depends on the order of the uses }
  TRALWinTime = record
    wYear, wMonth, wDayOfWeek, wDay, wHour, wMinute, wSecond, wMilliseconds: Word;
  end;

function RALTzInfoForYear(AYear: Word; ADynamic: Pointer;
  var ATimeZone: TTimeZoneInformation): BOOL; stdcall;
  external 'kernel32.dll' name 'GetTimeZoneInformationForYear';
function RALTzLocalToUtc(ATimeZone: PTimeZoneInformation; const ALocal: TRALWinTime;
  var AUtc: TRALWinTime): BOOL; stdcall;
  external 'kernel32.dll' name 'TzSpecificLocalTimeToSystemTime';
function RALTzUtcToLocal(ATimeZone: PTimeZoneInformation; const AUtc: TRALWinTime;
  var ALocal: TRALWinTime): BOOL; stdcall;
  external 'kernel32.dll' name 'SystemTimeToTzSpecificLocalTime';

threadvar
  { the rules of one year, kept per thread: this runs for every JWT checked
    and every cookie written, and Windows may read them from the registry }
  gZoneYear: Word;
  gZone: TTimeZoneInformation;

{ ALocalIn: ADateTime is local and UTC is wanted, or the other way round }
function RALWinConvert(ADateTime: TDateTime; ALocalIn: boolean; out AResult: TDateTime): boolean;
var
  vIn, vOut: TRALWinTime;
begin
  FillChar(vIn, SizeOf(vIn), 0);
  DecodeDateTime(ADateTime, vIn.wYear, vIn.wMonth, vIn.wDay, vIn.wHour, vIn.wMinute,
    vIn.wSecond, vIn.wMilliseconds);
  Result := gZoneYear = vIn.wYear;
  if not Result then
  begin
    Result := RALTzInfoForYear(vIn.wYear, nil, gZone);
    if Result then
      gZoneYear := vIn.wYear;
  end;
  if Result then
    if ALocalIn then
      Result := RALTzLocalToUtc(@gZone, vIn, vOut)
    else
      Result := RALTzUtcToLocal(@gZone, vIn, vOut);
  if Result then
    Result := TryEncodeDateTime(vOut.wYear, vOut.wMonth, vOut.wDay, vOut.wHour,
      vOut.wMinute, vOut.wSecond, vOut.wMilliseconds, AResult);
end;
{$IFEND}

{$IF NOT DEFINED(FPC) AND NOT DEFINED(DELPHIXE2UP)}
{ Delphi XE: minutes to add to local time to get UTC, as Windows has them now -
  only for when it will not say for the date. The old code here subtracted
  them, and kept them in a Cardinal, which a zone east of UTC turns negative }
function RALCurrentBias: Integer;
var
  vZone: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(vZone) of
    TIME_ZONE_ID_UNKNOWN:
      Result := vZone.Bias;
    TIME_ZONE_ID_STANDARD:
      Result := vZone.Bias + vZone.StandardBias;
    TIME_ZONE_ID_DAYLIGHT:
      Result := vZone.Bias + vZone.DaylightBias;
  else
    Result := 0;
  end;
end;
{$IFEND}

{ Two hours of the year are not one instant each, and both compilers settle
  them alike, so a token or a cookie carries the same hour whichever wrote it.
  The hour the clocks skip forward does not exist: it is read with the offset
  in force before the change - taken from the day before - which moves it
  forward by the hour skipped (Windows reads it with the offset after the
  change, an hour earlier). The hour the clocks repeat happens twice: the
  first, still in daylight time, is the one taken (the Delphi RTL takes the
  second unless told). The same choices as java.time and Python }
function RALDateTimeToGMT(ADateTime: TDateTime): TDateTime;
{$IF (DEFINED(FPC) OR NOT DEFINED(DELPHIXE2UP)) AND DEFINED(RALWindows)}
var
  vBack, vBefore: TDateTime;
{$IFEND}
begin
  {$IF DEFINED(FPC) OR NOT DEFINED(DELPHIXE2UP)}
    {$IFDEF RALWindows}
    if RALWinConvert(ADateTime, True, Result) then
    begin
      { only a skipped hour comes back as another local time }
      if RALWinConvert(Result, False, vBack) and not SameDateTime(vBack, ADateTime) and
         RALWinConvert(ADateTime - 1, True, vBefore) then
        Result := ADateTime + (vBefore - (ADateTime - 1));
      Exit;
    end;
    {$ENDIF}
    {$IFDEF FPC}
    Result := LocalTimeToUniversal(ADateTime);
    {$ELSE}
    Result := IncMinute(ADateTime, RALCurrentBias);
    {$ENDIF}
  {$ELSE}
    { ToUniversalTime raises on the skipped hour - an opensql over a record
      holding one answered 500 }
    if TTimeZone.Local.IsInvalidTime(ADateTime) then
      Result := ADateTime + (TTimeZone.Local.ToUniversalTime(ADateTime - 1) -
                             (ADateTime - 1))
    else
      Result := TTimeZone.Local.ToUniversalTime(ADateTime, True);
  {$IFEND}
end;

function RALGMTToDateTime(ADateTime: TDateTime): TDateTime;
begin
  {$IF DEFINED(FPC) OR NOT DEFINED(DELPHIXE2UP)}
    {$IFDEF RALWindows}
    if not RALWinConvert(ADateTime, False, Result) then
    {$ENDIF}
      {$IFDEF FPC}
      Result := UniversalTimeToLocal(ADateTime);
      {$ELSE}
      Result := IncMinute(ADateTime, -RALCurrentBias);
      {$ENDIF}
  {$ELSE}
    Result := TTimeZone.Local.ToLocalTime(ADateTime);
  {$IFEND}
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
begin
  { it read fixed offsets of the IMF date alone, and StrToInt raised on
    anything else - a one-digit day, RFC 850, asctime, a cookie's dashes }
  if not RALTryHTTPDate(AStr, Result) then
    raise EConvertError.CreateFmt(emHTTPDateInvalid, [string(AStr)]);
end;

function RALTryHTTPDate(const AText: StringRAL; out AUtc: TDateTime): Boolean;
const
  cMonths: array[0..35] of AnsiChar = 'janfebmaraprmayjunjulaugsepoctnovdec';
var
  vText: PByte;
  vLen, vPos, vStart, vEnd, vInt: IntegerRAL;
  vDay, vMonth, vYear, vHour, vMin, vSec, vH, vM, vS: Integer;

  { RFC 6265 5.1.1: %x09 / %x20-2F / %x3B-40 / %x5B-60 / %x7B-7E. ':' is
    not one, so a time stays one token }
  function IsDelimiter(AChr: Byte): Boolean;
  begin
    Result := (AChr = 9) or ((AChr >= $20) and (AChr <= $2F)) or
              ((AChr >= $3B) and (AChr <= $40)) or
              ((AChr >= $5B) and (AChr <= $60)) or
              ((AChr >= $7B) and (AChr <= $7E));
  end;

  { AMin to AMax digits from APos, and not one more: their value, APos past
    them. -1 when the token does not start that way }
  function Number(var APos: IntegerRAL; AMin, AMax: IntegerRAL): Integer;
  var
    vCount: IntegerRAL;
  begin
    Result := 0;
    vCount := 0;
    while (APos < vEnd) and (vText[APos] >= Ord('0')) and (vText[APos] <= Ord('9')) and
          (vCount <= AMax) do
    begin
      Result := Result * 10 + (vText[APos] - Ord('0'));
      Inc(APos);
      Inc(vCount);
    end;
    if (vCount < AMin) or (vCount > AMax) then
      Result := -1;
  end;

  function SameMonth(AIndex: IntegerRAL): Boolean;
  var
    vChr: IntegerRAL;
  begin
    Result := True;
    for vChr := 0 to 2 do
      if (vText[vStart + vChr] or $20) <> Ord(cMonths[AIndex * 3 + vChr]) then
      begin
        Result := False;
        Exit;
      end;
  end;

begin
  Result := False;
  AUtc := 0;
  vDay := -1;
  vMonth := -1;
  vYear := -1;
  vHour := -1;
  vMin := 0;
  vSec := 0;

  { each token is tried as the time, then the day, the month and the year,
    whichever of them is still missing - the first that fits takes it }
  vText := PByte(Pointer(AText));
  vLen := Length(AText);
  vPos := 0;
  while vPos < vLen do
  begin
    while (vPos < vLen) and IsDelimiter(vText[vPos]) do
      Inc(vPos);
    vStart := vPos;
    while (vPos < vLen) and not IsDelimiter(vText[vPos]) do
      Inc(vPos);
    vEnd := vPos;
    if vStart = vEnd then
      Continue;

    if vHour < 0 then
    begin
      vInt := vStart;
      vH := Number(vInt, 1, 2);
      if (vH >= 0) and (vInt < vEnd) and (vText[vInt] = Ord(':')) then
      begin
        Inc(vInt);
        vM := Number(vInt, 1, 2);
        if (vM >= 0) and (vInt < vEnd) and (vText[vInt] = Ord(':')) then
        begin
          Inc(vInt);
          vS := Number(vInt, 1, 2);
          if vS >= 0 then
          begin
            vHour := vH;
            vMin := vM;
            vSec := vS;
            Continue;
          end;
        end;
      end;
    end;

    if vDay < 0 then
    begin
      vInt := vStart;
      vDay := Number(vInt, 1, 2);
      if vDay >= 0 then
        Continue;
    end;

    if (vMonth < 0) and (vEnd - vStart >= 3) then
    begin
      for vInt := 0 to 11 do
        if SameMonth(vInt) then
        begin
          vMonth := vInt + 1;
          Break;
        end;
      if vMonth > 0 then
        Continue;
    end;

    if vYear < 0 then
    begin
      vInt := vStart;
      vYear := Number(vInt, 2, 4);
    end;
  end;

  if (vHour < 0) or (vDay < 0) or (vMonth < 0) or (vYear < 0) then
    Exit;
  if vYear <= 69 then
    Inc(vYear, 2000)
  else if vYear <= 99 then
    Inc(vYear, 1900);
  if (vDay < 1) or (vDay > 31) or (vYear < 1601) or (vHour > 23) or
     (vMin > 59) or (vSec > 59) then
    Exit;
  Result := TryEncodeDateTime(vYear, vMonth, vDay, vHour, vMin, vSec, 0, AUtc);
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

initialization
  RALInvariantFormat := {$IFDEF FPC}DefaultFormatSettings{$ELSE}FormatSettings{$ENDIF};
  RALInvariantFormat.DecimalSeparator := '.';
  RALInvariantFormat.ThousandSeparator := ',';
  {$IF DEFINED(FPC) AND NOT DEFINED(CPU64)}
  System.InitCriticalSection(gAtomic64);
  {$IFEND}

{$IF (DEFINED(FPC) AND NOT DEFINED(CPU64)) OR NOT DEFINED(RALWindows)}
finalization
  {$IF DEFINED(FPC) AND NOT DEFINED(CPU64)}
  System.DoneCriticalSection(gAtomic64);
  {$IFEND}
  {$IFNDEF RALWindows}
  if gURandom >= 0 then
    FileClose(THandle(gURandom));
  {$ENDIF}
{$IFEND}

end.
