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

implementation

const
  { method names without the 'am' prefix, in the order of the enum }
  RALMethodNames: array [TRALMethod] of StringRAL = (
    'ALL', 'GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS', 'HEAD', 'TRACE');

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
    raise Exception.Create('RandomBytes: RtlGenRandom failed');
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
    raise EConvertError.Create('Mês inválido na data HTTP');

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

end.
