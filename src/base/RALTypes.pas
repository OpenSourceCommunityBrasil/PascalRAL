{ @abstract Unit for all type definitions used within PascalRAL
  These definitions are meant to keep same code across all versions of the IDE
  or IDEs that might differ on the charset code or basic type length.
  Expect heavy usage of IFDEFs in this unit
}
unit RALTypes;

interface

{$I ..\base\PascalRAL.inc}

uses
  {$IFDEF FPC}
  bufstream,
  {$ENDIF}
  Classes, SysUtils,
  RALConsts;

type
  IntegerRAL = Integer;
  Int64RAL = Int64;
  DoubleRAL = Double;

  {$IF Defined(FPC) OR Defined(DELPHIXE3UP)}
    UInt64RAL = UInt64;
  {$ELSE}
    UInt64RAL = Int64;
  {$IFEND}

  {$IF Defined(FPC) OR NOT Defined(DELPHIXE3UP)}
    StringRAL = string;
    CharRAL = Char;
  {$ELSE}
    StringRAL = WideString;
    CharRAL = WideChar;
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
  TRALSecurityOption = (rsoBruteForceProtection, rsoFloodProtection,
    rsoPathTransvBlackList);
  TRALSecurityOptions = set of TRALSecurityOption;
  TRALExecBehavior = (ebSingleThread, ebMultiThread);
  TRALDateTimeFormat = (dtfUnix, dtfISO8601, dtfCustom);

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

// Returns the last position of a string
function RALHighStr(const AStr: StringRAL): integer;

function StringToBytesUTF8(const AString: StringRAL): TBytes;
function BytesToStringUTF8(const ABytes: TBytes): StringRAL;

{$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE6UP)}
function DateToISO8601(const AValue: TDateTime): StringRAL;
function ISO8601ToDate(const AValue: StringRAL): TDateTime;
{$IFEND}

implementation

function RALHighStr(const AStr: StringRAL): integer;
begin
  {$IF NOT Defined(FPC) AND NOT Defined(DELPHIXE3UP)}
  Result := Length(AStr);
  {$ELSE}
  Result := High(AStr);
  {$IFEND}
end;

function StringToBytesUTF8(const AString: StringRAL): TBytes;
{$IFNDEF HAS_Encoding}
  var
    vStr : ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
    Result := TEncoding.UTF8.GetBytes(AString);
  {$ELSE}
    vStr := UTF8Encode(AString);
    SetLength(Result, Length(vStr));
    Move(vStr[POSINISTR], Result[0], Length(vStr));
  {$ENDIF}
end;

function BytesToStringUTF8(const ABytes: TBytes): StringRAL;
{$IFNDEF HAS_Encoding}
  var
    vStr : ansistring;
{$ENDIF}
begin
  {$IFDEF HAS_Encoding}
    Result := TEncoding.UTF8.GetString(ABytes);
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

end.
