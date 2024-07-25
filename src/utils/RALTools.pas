/// Unit for General public functions
unit RALTools;

{$I ..\base\PascalRAL.inc}

interface

uses
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
function Contains(const AStr: StringRAL; const AArray: array of StringRAL): boolean;

implementation

function FixRoute(ARoute: StringRAL): StringRAL;
begin
  Result := '/' + ARoute;

  // path transversal fix
  Result := StringReplace(Result, '../', '', [rfReplaceAll]);

  while Pos(StringRAL('//'), Result) > 0 do
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);

  if (Result <> '') and (Result <> '/') and (Result[RALHighStr(Result)] = '/') then
    Delete(Result, RALHighStr(Result), 1);
end;

function RandomBytes(numOfBytes: IntegerRAL): TBytes;
var
  vInt: IntegerRAL;
begin
  SetLength(Result, numOfBytes);
  Randomize;
  for vInt := 1 to numOfBytes do
    Result[vInt - 1] := Random(256);
end;

function HTTPMethodToRALMethod(AMethod: StringRAL): TRALMethod;
var
  vInt: IntegerRAL;
begin
  AMethod := 'am' + UpperCase(AMethod);
  vInt := GetEnumValue(TypeInfo(TRALMethod), AMethod);
  if vInt <> -1 then
    Result := TRALMethod(vInt)
  else
    Result := amGET;
end;

function RALMethodToHTTPMethod(AMethod: TRALMethod): StringRAL;
begin
  Result := GetEnumName(TypeInfo(TRALMethod), Ord(AMethod));
  Delete(Result, 1, 2); // delete 'am'
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
    if CharInSet(AValue[vInt], ['0'..'9']) then
      Result := Result + AValue[vInt];
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
      Result := TDateTime(0);
  end
  else
  begin
    if not TryEncodeDateTime(wAno, wMes, wDia, wHor, wMin, wSeg, wMil, Result) then
      Result := TDateTime(0);
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

function Contains(const AStr: StringRAL; const AArray: array of StringRAL): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to Pred(Length(AArray)) do
    if SameText(AStr, AArray[I]) then
    begin
      Result := True;
      Break;
    end;
end;

end.
